      *================================================================*
      * PROGRAM  : SBNRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAVINGS BOND SERVICES - RENEWAL. READS THE SBN      *
      *            TRANSACTIONS, UPDATES THE SBN MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. US SAVINGS BOND SALES,          *
      *            REDEMPTION AND TAX REPORTING FOR CUSTOMERS          *
      * INPUT    : SBNMST (KSDS), SBNTRN, BANKCTL                      *
      * OUTPUT   : SBNOUT, SBNRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SBLIM, SBFEE, SBPEN     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-09-13 EWS  ORIGINAL                                       *
      * 1994-08-02 GAF  PURCHASE TABLE EXPANDED CHG8758                *
      * 1998-01-05 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-09-22 RJT  INTEREST STATUS CODES ADDED INC2306            *
      * 2000-06-21 DPS  TAXABLE CALCULATION CORRECTED CHG8757          *
      * 2001-10-10 PQR  PERFORMANCE - REDEEM READ AHEAD CHG3842        *
      * 2002-08-03 DPS  REDEEM STATUS CODES ADDED REQ2011              *
      * 2005-07-08 SKN  PURCHASE FEE SCHEDULE UPDATE INC4496           *
      * 2008-03-08 DPS  REDEEM CALCULATION CORRECTED REQ8904           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBNRNEW.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "SBNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SBNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "SBNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "SBNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SBNRNRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY SBNMST.
       FD  TRAN-FILE.
       COPY SBNTRN.
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
                                           VALUE 17.000000.
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
      * WS-ISSUE-PR-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUE-PR-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000780500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000168000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000876500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000779000'.
       01  WS-ISSUE-PR-SCHED-TBL REDEFINES WS-ISSUE-PR-SCHED-VALUES.
           05  WS-ISSUE-PR-SCHED-ROW OCCURS 4 TIMES.
               10  WS-ISSUE-PR-SCHED-LOW   PIC 9(09)V99.
               10  WS-ISSUE-PR-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUE-PR-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUE-PR-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000007500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000058000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000575000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000428000'.
       01  WS-ISSUE-PR-SCHEDX-TBL REDEFINES WS-ISSUE-PR-SCHEDX-VALUES.
           05  WS-ISSUE-PR-SCHEDX-ROW OCCURS 5 TIMES.
               10  WS-ISSUE-PR-SCHEDX-LOW  PIC 9(09)V99.
               10  WS-ISSUE-PR-SCHEDX-RATE PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000301000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000477500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000867000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000155000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000903000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000510000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011250000000570500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022250000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000062000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000618500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024500000000160000'.
       01  WS-PURCHASE-SCHED-TBL REDEFINES WS-PURCHASE-SCHED-VALUES.
           05  WS-PURCHASE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-PURCHASE-SCHED-LOW   PIC 9(09)V99.
               10  WS-PURCHASE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAXABLE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAXABLE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000492000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000518500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000372500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000224500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000831500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000834000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018850000000568500'.
       01  WS-TAXABLE-GRID-TBL REDEFINES WS-TAXABLE-GRID-VALUES.
           05  WS-TAXABLE-GRID-ROW OCCURS 9 TIMES.
               10  WS-TAXABLE-GRID-LOW     PIC 9(09)V99.
               10  WS-TAXABLE-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SBNRNEW  SAVINGS BOND SERVICES'.
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
       COPY SBLIMP.
       COPY SBFEEP.
       COPY SBPENP.

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
               MOVE 'SBNMST' TO ABND-FILE-NAME
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
           MOVE SBNT-KEY TO SBNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-PENALTY-PCT
               PERFORM 3010-AGE-POST
               PERFORM 3020-SPLIT-FACE
               PERFORM 3030-SPLIT-FACE
               PERFORM 3040-COUNT-EXCEPT
               PERFORM 3050-ASSESS-OVERRIDE
               PERFORM 3060-EDIT-REJECT
               PERFORM 3070-CHECK-LAST-MAINT
               PERFORM 3080-DESCRIBE-STATUS
               PERFORM 3090-SPLIT-REDEEM
               PERFORM 3100-ASSESS-OVERRIDE
               PERFORM 3110-LIMIT-TAXABLE
               PERFORM 3120-ACCRUE-REDEEM
               PERFORM 3130-DESCRIBE-SERIES
               PERFORM 3140-FLAG-ISSUE-PRICE
               PERFORM 3150-ACCRUE-ISSUE-PRICE
               PERFORM 3160-ACCRUE-ISSUE-PRICE
               PERFORM 3170-AGE-MATURITY
               PERFORM 3180-CHECK-POST
               PERFORM 3190-ACCUM-ISSUE-TYPE
               PERFORM 3200-EDIT-REF-NO
               PERFORM 3210-CHECK-OPEN
               PERFORM 3220-LIMIT-REDEEM
               PERFORM 3230-COUNT-SEQ
               PERFORM 3240-ACCUM-CHANNEL
               PERFORM 3250-LIMIT-REDEEM
               PERFORM 3260-AGE-LAST-PAY
               PERFORM 3270-CHECK-POST
               PERFORM 3280-FLAG-PURCHASE
               PERFORM 3290-SELECT-PENALTY-PCT
               PERFORM 3300-SET-STATUS
               PERFORM 3310-ASSESS-NOTICE-SENT
               PERFORM 3320-ACCUM-CHANNEL
               PERFORM 3330-SELECT-FEE-PCT
               PERFORM 3340-AGE-MATURITY
               PERFORM 3350-EDIT-REJECT
               PERFORM 3360-CHECK-EXPIRY
               PERFORM 3370-ACCUM-STATUS
               PERFORM 3380-ACCUM-ISSUE-TYPE
               PERFORM 3390-EDIT-CONTACT
               PERFORM 3400-COUNT-EXCEPT
               PERFORM 3410-DESCRIBE-REASON
               PERFORM 3420-EDIT-REJECT
               PERFORM 3430-FLAG-FACE
               PERFORM 3440-COUNT-SEQ
               PERFORM 3450-ASSESS-REVIEW
               PERFORM 3460-SPLIT-PURCHASE
               PERFORM 3470-ACCUM-REASON
               PERFORM 3480-SELECT-MARGIN
               PERFORM 3490-COUNT-EXCEPT
               PERFORM 3500-EDIT-REJECT
               PERFORM 3510-ACCRUE-REDEEM
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-COUNT-CYCLE
               PERFORM 3540-COUNT-RETRY
               PERFORM 3550-FLAG-ISSUE-PRICE
               PERFORM 3560-DESCRIBE-SERIES
               PERFORM 3570-DESCRIBE-ISSUE-TYPE
               PERFORM 3580-COUNT-PAYMENT
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-DESCRIBE-SERIES
               PERFORM 3610-AGE-OPEN
               PERFORM 3620-SELECT-MARGIN
               PERFORM 3630-ACCRUE-TAXABLE
               PERFORM 3640-ACCRUE-REDEEM
               PERFORM 3650-LIMIT-INTEREST
               PERFORM 3660-AGE-NEXT-REVIEW
               PERFORM 3670-SELECT-FEE-PCT
               PERFORM 3680-EDIT-CUST-ID
               PERFORM 3690-AGE-POST
               PERFORM 3700-AGE-OPEN
               PERFORM 3710-SELECT-MARGIN
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-FLAG-REDEEM
               PERFORM 3740-SELECT-MARGIN
               PERFORM 3750-ASSESS-REVIEW
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-LIMIT-TAXABLE
               PERFORM 3780-SET-TRAN
               PERFORM 3790-ACCUM-SERIES
               PERFORM 3800-SELECT-PENALTY-PCT
               PERFORM 3810-ASSESS-NOTICE-SENT
               PERFORM 3820-PRORATE-PURCHASE
               PERFORM 3830-ACCUM-TRAN
               PERFORM 3840-ACCUM-BRANCH
               PERFORM 3850-LIMIT-ISSUE-PRICE
               PERFORM 3860-ASSESS-HOLD
               PERFORM 3870-PRORATE-ISSUE-PRICE
               PERFORM 3880-SELECT-PENALTY-PCT
               PERFORM 3890-PRORATE-ACCRUAL
               PERFORM 3900-ASSESS-REVIEW
               PERFORM 3910-ACCUM-STATUS
               PERFORM 3920-EDIT-CONTACT
               PERFORM 3930-SET-CHANNEL
               PERFORM 3940-FLAG-TAXABLE
               PERFORM 3950-ACCRUE-TAXABLE
               PERFORM 3960-CHECK-LAST-PAY
               PERFORM 3970-FLAG-ACCRUAL
               PERFORM 3980-LIMIT-TAXABLE
               PERFORM 3990-SPLIT-INTEREST
               PERFORM 4000-FLAG-TAXABLE
               PERFORM 4010-PRORATE-INTEREST
               PERFORM 4020-COUNT-PAYMENT
               PERFORM 4030-SELECT-PENALTY-PCT
               PERFORM 4040-ACCRUE-INTEREST
               PERFORM 4050-SET-ISSUE-TYPE
               PERFORM 4060-AGE-NOTICE
               PERFORM 4070-ASSESS-AUTO-RENEW
               PERFORM 4080-CHECK-EFFECTIVE
               PERFORM 4090-SPLIT-FACE
               PERFORM 4100-CHECK-LAST-MAINT
               PERFORM 4110-ACCRUE-INTEREST
               PERFORM 4120-CHECK-LAST-MAINT
               PERFORM 4130-DESCRIBE-BRANCH
               PERFORM 4140-AGE-NOTICE
               PERFORM 4150-AGE-LAST-MAINT
               PERFORM 4160-ASSESS-HOLD
               PERFORM 4170-ACCRUE-INTEREST
               PERFORM 4180-FLAG-TAXABLE
               PERFORM 4190-PRORATE-ACCRUAL
               PERFORM 4200-FLAG-FACE
               PERFORM 4210-ASSESS-REVIEW
               PERFORM 4220-LIMIT-INTEREST
               PERFORM 4230-EDIT-OPERATOR
               PERFORM 4240-CHECK-NEXT-REVIEW
               PERFORM 4250-FLAG-TAXABLE
               PERFORM 4260-DESCRIBE-BRANCH
               PERFORM 4270-EDIT-CONTACT
               PERFORM 4280-DESCRIBE-TRAN
               PERFORM 4290-COUNT-RETRY
               PERFORM 4300-DESCRIBE-REASON
               PERFORM 4310-AGE-LAST-PAY
               PERFORM 4320-SELECT-PENALTY-PCT
               PERFORM 4330-EDIT-CONTACT
               PERFORM 4340-SELECT-PENALTY-PCT
               PERFORM 4350-ACCUM-SERIES
               PERFORM 4360-PRORATE-REDEEM
               PERFORM 4370-EDIT-OPERATOR
               PERFORM 4380-SET-SERIES
               PERFORM 4390-ACCUM-SOURCE
               PERFORM 4400-ACCRUE-REDEEM
               PERFORM 4410-AGE-LAST-PAY
               PERFORM 4420-SPLIT-ACCRUAL
               PERFORM 4430-SPLIT-INTEREST
               PERFORM 4440-EDIT-NAME
               PERFORM 4450-LIMIT-TAXABLE
               PERFORM 4460-DESCRIBE-BRANCH
               PERFORM 4470-SPLIT-TAXABLE
               PERFORM 4480-ASSESS-HOLD
               PERFORM 4490-ACCRUE-FACE
               PERFORM 4500-DESCRIBE-SERIES
               PERFORM 4510-EDIT-CONTACT
               PERFORM 4520-AGE-EFF
               PERFORM 4530-COUNT-PAYMENT
               PERFORM 4540-SPLIT-PURCHASE
               PERFORM 4550-SPLIT-PURCHASE
               PERFORM 7020-CALL-SBLIM
               PERFORM 7030-CALL-SBFEE
               PERFORM 7000-CALL-SBPEN
               REWRITE SBN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'SBNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM SBN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SBNM-ACCRUAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3050-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNT-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 20.00 TO SBNT-TAXABLE-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 20.00 TO SBNT-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = WS-WORK-AMT-2 *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNT-TAXABLE-AMT > 0 AND SBNT-TAXABLE-AMT < 5.00
               MOVE 5.00 TO SBNT-TAXABLE-AMT
           END-IF
           IF SBNT-TAXABLE-AMT > 35.00
               MOVE 35.00 TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E272 REJECT CD REQUIRED
      * E654 REDEEM YTD NEGATIVE
      * E867 REDEEM YTD OVER 1000000
      * E297 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 1000000.00
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-REDEEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE SBNM-REDEEM-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE ISSUE PRICE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3100-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SBNM-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-FEE-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 3.00 TO SBNM-ACCRUAL-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 3.00 TO SBNM-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNM-ACCRUAL-AMT ROUNDED =
                       SBNM-ISSUE-PRICE-YTD * SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-ACCRUAL-AMT > 0 AND SBNM-ACCRUAL-AMT < 2.00
               MOVE 2.00 TO SBNM-ACCRUAL-AMT
           END-IF
           IF SBNM-ACCRUAL-AMT > 35.00
               MOVE 35.00 TO SBNM-ACCRUAL-AMT
           END-IF
           ADD SBNM-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3110-LIMIT-TAXABLE.
           IF SBNT-TAXABLE-AMT < 10.00
               MOVE 10.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-TAXABLE-AMT > 500.00
               MOVE 500.00 TO SBNT-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-REDEEM.
           IF SBNM-REDEEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-YTD * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3140-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUE PRICE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-ISSUE-PRICE-AMT * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAXABLE AMT BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-TAXABLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-TAXABLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E032 REF NO REQUIRED
      * E301 REDEEM AMT NEGATIVE
      * E478 REDEEM AMT OVER 5000
      * E624 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-REF-NO = SPACES
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT < 0
                   MOVE 'E301' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT > 5000.00
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN SBNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-REDEEM.
           IF SBNT-REDEEM-AMT < 25.00
               MOVE 25.00 TO SBNT-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNT-REDEEM-AMT > 1000.00
               MOVE 1000.00 TO SBNT-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-CHANNEL-CD
                   ADD SBNM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-REDEEM.
           IF SBNM-REDEEM-AMT < 50.00
               MOVE 50.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-REDEEM-AMT > 1000.00
               MOVE 1000.00 TO SBNM-REDEEM-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-PURCHASE.
           IF SBNM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBNM-PURCHASE-AMT NOT < WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * CS -> HF WHEN LAST PAY DATE IS PAST
      * HF -> JZ WHEN LAST PAY DATE IS PAST
      * JZ -> CS WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-STATUS.
           EVALUATE SBNM-STATUS-CD
               WHEN 'CS'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CS' TO SBNM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE INTEREST AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3310-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBNM-INTEREST-AMT *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SBNM-INTEREST-AMT *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-CHANNEL-CD
                   ADD SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE GRID TIER FOR THE TAXABLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-GRID-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-TAXABLE-AMT NOT < WS-TAXABLE-GRID-LOW(WS-TX)
                   MOVE WS-TAXABLE-GRID-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-MATURITY-DATE > 0 AND SBNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E810 REJECT CD REQUIRED
      * E540 WORK AMT 2 NEGATIVE
      * E892 WORK AMT 2 OVER 1000000
      * E566 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN SBNT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUE PRICE AMT BY ISSUE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-ISSUE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-ISSUE-TYPE-CD
                   ADD SBNM-ISSUE-PRICE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-ISSUE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ISSUE-PRICE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E388 CONTACT REQUIRED
      * E424 REDEEM AMT NEGATIVE
      * E539 REDEEM AMT OVER 1000000
      * E474 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT < 0
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT > 1000000.00
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E566 REJECT CD REQUIRED
      * E027 PURCHASE YTD NEGATIVE
      * E299 PURCHASE YTD OVER 1000000
      * E246 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E566' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-YTD < 0
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-YTD > 1000000.00
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-FACE.
           IF SBNM-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-SEQ.
           IF SBNT-SEQ < 9999999
               ADD 1 TO SBNT-SEQ
           END-IF
           IF SBNT-SEQ NOT < 6
               MOVE 'Y' TO SBNM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FACE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3450-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-FACE-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNT-TRAN-CD = 'BY'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN SBNT-TRAN-CD = 'RD'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SBNM-FACE-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-REASON-CD
                   ADD SBNM-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAXABLE GRID TIER FOR THE TAXABLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAXABLE-GRID-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SBNM-TAXABLE-YTD NOT < WS-TAXABLE-GRID-LOW(WS-TX)
                   MOVE WS-TAXABLE-GRID-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E993 REJECT CD REQUIRED
      * E942 ACCRUAL AMT NEGATIVE
      * E805 ACCRUAL AMT OVER 99999
      * E812 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-REJECT-CD = SPACES
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-AMT < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN SBNM-ACCRUAL-AMT > 99999.00
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN SBNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-REDEEM.
           IF SBNM-REDEEM-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-YTD * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE REDEEM AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNT-REDEEM-AMT NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-CYCLE.
           IF SBNM-CYCLE-CNT < 9999999
               ADD 1 TO SBNM-CYCLE-CNT
           END-IF
           IF SBNM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 3
               MOVE 'Y' TO SBNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUE PRICE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-ISSUE-PRICE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SBNM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF SBNM-EXCEPT-CNT < 9999999
               ADD 1 TO SBNM-EXCEPT-CNT
           END-IF
           IF SBNM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO SBNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE TAXABLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-TAXABLE-AMT NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-TAXABLE.
           IF SBNM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * SBNM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO SBNM-ISSUE-PRICE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-INTEREST.
           IF SBNM-INTEREST-YTD < 0.00
               MOVE 0.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-INTEREST-YTD > 1000.00
               MOVE 1000.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-REVIEW-DATE > 0 AND SBNM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE REDEEM YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-REDEEM-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO SBNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E691 CUST ID REQUIRED
      * E297 INTEREST YTD NEGATIVE
      * E632 INTEREST YTD OVER 25000
      * E667 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CUST-ID = SPACES
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD < 0
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN SBNM-INTEREST-YTD > 25000.00
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SBNT-POST-DATE > 0 AND SBNT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-INTEREST-YTD NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 NAME REQUIRED
      * E707 ISSUE PRICE YTD NEGATIVE
      * E222 ISSUE PRICE YTD OVER 10000
      * E693 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD < 0
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN SBNM-ISSUE-PRICE-YTD > 10000.00
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN SBNM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REDEEM YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-REDEEM.
           IF SBNM-REDEEM-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-REDEEM-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE TAXABLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBNM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-TAXABLE-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO SBNM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3750-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNT-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-HOLD-FLAG = 'Y'
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-FEE-PCT / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 20.00 TO SBNT-TAXABLE-AMT
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 5.00 TO SBNT-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = WS-WORK-AMT *
                       SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNT-TAXABLE-AMT > 0 AND SBNT-TAXABLE-AMT < 5.00
               MOVE 5.00 TO SBNT-TAXABLE-AMT
           END-IF
           IF SBNT-TAXABLE-AMT > 150.00
               MOVE 150.00 TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAXABLE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-AMT < 50.00
               MOVE 50.00 TO SBNM-TAXABLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-AMT > 500.00
               MOVE 500.00 TO SBNM-TAXABLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BY -> RD WHEN NEXT DUE DATE IS PAST
      * RD -> IR WHEN NEXT DUE DATE IS PAST
      * IR -> TX WHEN NEXT DUE DATE IS PAST
      * TX -> AD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IR' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'IR'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF SBNM-NEXT-DUE-DATE > 0 AND SBNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO SBNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE ISSUE PRICE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3810-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-FEE-PCT / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 12.00 TO SBNM-PURCHASE-YTD
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 15.00 TO SBNM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                       SBNM-ISSUE-PRICE-AMT * SBNM-FEE-PCT / 100
           END-EVALUATE
           IF SBNM-PURCHASE-YTD > 0 AND SBNM-PURCHASE-YTD < 5.00
               MOVE 5.00 TO SBNM-PURCHASE-YTD
           END-IF
           IF SBNM-PURCHASE-YTD > 75.00
               MOVE 75.00 TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-PURCHASE-YTD ROUNDED =
                   SBNM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNT-TRAN-CD
                   ADD SBNM-ACCRUAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ACCRUAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUAL YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-BRANCH-CD
                   ADD SBNM-ACCRUAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-ACCRUAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ISSUE PRICE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-ISSUE-PRICE.
           IF SBNM-ISSUE-PRICE-YTD < 10.00
               MOVE 10.00 TO SBNM-ISSUE-PRICE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-ISSUE-PRICE-YTD > 10000.00
               MOVE 10000.00 TO SBNM-ISSUE-PRICE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE FEE ON THE TAXABLE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3860-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNT-TAXABLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-TAXABLE-YTD *
                       SBNM-PENALTY-PCT / 200
               WHEN SBNM-STATUS-CD = 'CS'
                   MOVE 7.00 TO SBNT-TAXABLE-AMT
               WHEN SBNM-STATUS-CD = 'HF'
                   MOVE 3.00 TO SBNT-TAXABLE-AMT
               WHEN OTHER
                   COMPUTE SBNT-TAXABLE-AMT ROUNDED = SBNM-TAXABLE-YTD *
                       SBNM-PENALTY-PCT / 100
           END-EVALUATE
           IF SBNT-TAXABLE-AMT > 0 AND SBNT-TAXABLE-AMT < 1.00
               MOVE 1.00 TO SBNT-TAXABLE-AMT
           END-IF
           IF SBNT-TAXABLE-AMT > 35.00
               MOVE 35.00 TO SBNT-TAXABLE-AMT
           END-IF
           ADD SBNT-TAXABLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUE PRICE YTD.
      *----------------------------------------------------------------*
       3870-PRORATE-ISSUE-PRICE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ISSUE-PRICE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ISSUE-PRICE-YTD
           END-IF
           ADD SBNM-ISSUE-PRICE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE TAXABLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNM-TAXABLE-AMT NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REDEEM AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUAL AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-ACCRUAL-AMT ROUNDED =
                   SBNM-REDEEM-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE TAXABLE YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3900-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-TAXABLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE SBNM-TAXABLE-YTD ROUNDED = SBNM-INTEREST-AMT
                       * SBNM-MARGIN / 200
               WHEN SBNM-BRANCH-CD = 'BD'
                   MOVE 15.00 TO SBNM-TAXABLE-YTD
               WHEN SBNM-BRANCH-CD = 'FC'
                   MOVE 5.00 TO SBNM-TAXABLE-YTD
               WHEN OTHER
                   COMPUTE SBNM-TAXABLE-YTD ROUNDED = SBNM-INTEREST-AMT
                       * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-TAXABLE-YTD > 0 AND SBNM-TAXABLE-YTD < 10.00
               MOVE 10.00 TO SBNM-TAXABLE-YTD
           END-IF
           IF SBNM-TAXABLE-YTD > 250.00
               MOVE 250.00 TO SBNM-TAXABLE-YTD
           END-IF
           ADD SBNM-TAXABLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-STATUS-CD
                   ADD SBNM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E904 CONTACT REQUIRED
      * E961 REDEEM YTD NEGATIVE
      * E051 REDEEM YTD OVER 99999
      * E425 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 99999.00
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-CHANNEL.
           EVALUATE SBNT-CHANNEL-CD
               WHEN 'BRN'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SBNM-EFFECTIVE-DATE > 0 AND SBNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SBNT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAXABLE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3940-FLAG-TAXABLE.
           IF SBNT-TAXABLE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNT-TAXABLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAXABLE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-TAXABLE.
           IF SBNM-TAXABLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-REDEEM-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUAL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-ACCRUAL.
           IF SBNM-ACCRUAL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-ACCRUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 0.00
               MOVE 0.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 50000.00
               MOVE 50000.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBNT-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SBNT-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE SBNM-ACCRUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4000-FLAG-TAXABLE.
           IF SBNT-TAXABLE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNT-TAXABLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNT-INTEREST-AMT ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNT-INTEREST-AMT
           END-IF
           ADD SBNT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SBNM-PURCHASE-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-INTEREST.
           IF SBNM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUE TYPE CD:
      * CW -> DY WHEN OPEN DATE IS PAST
      * DY -> JA WHEN OPEN DATE IS PAST
      * JA -> KT WHEN OPEN DATE IS PAST
      * KT -> MF WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-ISSUE-TYPE.
           EVALUATE SBNM-ISSUE-TYPE-CD
               WHEN 'CW'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DY' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DY'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KT'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MF' TO SBNM-ISSUE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE REDEEM AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4070-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SBNM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNT-REDEEM-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNM-SERIES-CD = 'DD'
                   MOVE 20.00 TO SBNM-PURCHASE-AMT
               WHEN SBNM-SERIES-CD = 'DF'
                   MOVE 5.00 TO SBNM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-AMT ROUNDED = SBNT-REDEEM-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-PURCHASE-AMT > 0 AND SBNM-PURCHASE-AMT < 1.00
               MOVE 1.00 TO SBNM-PURCHASE-AMT
           END-IF
           IF SBNM-PURCHASE-AMT > 150.00
               MOVE 150.00 TO SBNM-PURCHASE-AMT
           END-IF
           ADD SBNM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = SBNM-FACE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-FACE-AMT - (WS-WORK-AMT * 12)
           COMPUTE SBNM-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-INTEREST.
           IF SBNM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-YTD * SBNM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO SBNM-TAXABLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NOTICE-DATE > 0 AND SBNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-TAXABLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-MAINT-DATE > 0 AND SBNM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNT-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-MARGIN / 200
               WHEN SBNM-REASON-CD = 'DG'
                   MOVE 12.00 TO SBNT-ACCRUAL-AMT
               WHEN SBNM-REASON-CD = 'UK'
                   MOVE 15.00 TO SBNT-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-ACCRUAL-AMT *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNT-ACCRUAL-AMT > 0 AND SBNT-ACCRUAL-AMT < 2.00
               MOVE 2.00 TO SBNT-ACCRUAL-AMT
           END-IF
           IF SBNT-ACCRUAL-AMT > 150.00
               MOVE 150.00 TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-INTEREST.
           IF SBNM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-INTEREST-AMT * SBNM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SBNT-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4180-FLAG-TAXABLE.
           IF SBNM-TAXABLE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SBNM-TAXABLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ACCRUAL YTD.
      *----------------------------------------------------------------*
       4190-PRORATE-ACCRUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-ACCRUAL-YTD ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SBNM-ACCRUAL-YTD
           END-IF
           ADD SBNM-ACCRUAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4200-FLAG-FACE.
           IF SBNM-FACE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SBNM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUAL FEE ON THE FACE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4210-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SBNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SBNT-ACCRUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-FACE-YTD *
                       SBNM-MARGIN / 200
               WHEN SBNM-ISSUE-TYPE-CD = 'CW'
                   MOVE 20.00 TO SBNT-ACCRUAL-AMT
               WHEN SBNM-ISSUE-TYPE-CD = 'DY'
                   MOVE 3.00 TO SBNT-ACCRUAL-AMT
               WHEN OTHER
                   COMPUTE SBNT-ACCRUAL-AMT ROUNDED = SBNM-FACE-YTD *
                       SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNT-ACCRUAL-AMT > 0 AND SBNT-ACCRUAL-AMT < 10.00
               MOVE 10.00 TO SBNT-ACCRUAL-AMT
           END-IF
           IF SBNT-ACCRUAL-AMT > 50.00
               MOVE 50.00 TO SBNT-ACCRUAL-AMT
           END-IF
           ADD SBNT-ACCRUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4220-LIMIT-INTEREST.
           IF SBNM-INTEREST-YTD < 10.00
               MOVE 10.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-INTEREST-YTD > 500.00
               MOVE 500.00 TO SBNM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E398 OPERATOR REQUIRED
      * E604 REDEEM AMT NEGATIVE
      * E940 REDEEM AMT OVER 25000
      * E282 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-AMT > 25000.00
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN SBNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SBNM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SBNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4250-FLAG-TAXABLE.
           IF SBNM-TAXABLE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SBNM-TAXABLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E126 CONTACT REQUIRED
      * E433 WORK AMT 2 NEGATIVE
      * E084 WORK AMT 2 OVER 250000
      * E795 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-TRAN.
           EVALUATE SBNT-TRAN-CD
               WHEN 'BY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'IR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-RETRY.
           IF SBNM-RETRY-CNT < 9999999
               ADD 1 TO SBNM-RETRY-CNT
           END-IF
           IF SBNM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SBNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-REASON.
           EVALUATE SBNM-REASON-CD
               WHEN 'DG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-ACCRUAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHEDX TIER FOR THE ACCRUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHEDX-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SBNT-ACCRUAL-AMT NOT < WS-ISSUE-PR-SCHEDX-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHEDX-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E390 CONTACT REQUIRED
      * E791 REDEEM AMT NEGATIVE
      * E169 REDEEM AMT OVER 1000000
      * E017 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 1000000.00
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN SBNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUE PR SCHED TIER FOR THE ISSUE PRICE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUE-PR-SCHED-RATE(1) TO SBNM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SBNM-ISSUE-PRICE-YTD NOT <
                   WS-ISSUE-PR-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUE-PR-SCHED-RATE(WS-TX) TO
                       SBNM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SBNM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SBNM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY SERIES CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-SERIES.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SERIES-CD
                   ADD SBNM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SERIES-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAXABLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REDEEM YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-REDEEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SBNM-REDEEM-YTD ROUNDED =
                   SBNM-TAXABLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SBNM-REDEEM-YTD
           END-IF
           ADD SBNM-REDEEM-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E237 OPERATOR REQUIRED
      * E241 PURCHASE AMT NEGATIVE
      * E407 PURCHASE AMT OVER 250000
      * E978 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNT-OPERATOR = SPACES
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT < 0
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN SBNM-PURCHASE-AMT > 250000.00
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN SBNM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SERIES CD:
      * DD -> DF WHEN OPEN DATE IS PAST
      * DF -> SS WHEN OPEN DATE IS PAST
      * SS -> VJ WHEN OPEN DATE IS PAST
      * VJ -> VX WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SS' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SS'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJ' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJ'
                   IF SBNM-OPEN-DATE > 0 AND SBNM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO SBNM-SERIES-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REDEEM AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4390-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SBNM-SOURCE-CD
                   ADD SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SBNM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SBNT-REDEEM-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REDEEM AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-REDEEM.
           IF SBNM-REDEEM-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-REDEEM-AMT * SBNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF SBNM-LAST-PAY-DATE > 0 AND SBNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ACCRUAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4420-SPLIT-ACCRUAL.
           COMPUTE WS-WORK-AMT = SBNM-ACCRUAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-ACCRUAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNM-PURCHASE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = SBNT-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNT-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNM-REDEEM-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E898 NAME REQUIRED
      * E125 REDEEM AMT NEGATIVE
      * E077 REDEEM AMT OVER 1000000
      * E078 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-NAME = SPACES
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT < 0
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN SBNT-REDEEM-AMT > 1000000.00
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN SBNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAXABLE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4450-LIMIT-TAXABLE.
           IF SBNM-TAXABLE-YTD < 25.00
               MOVE 25.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SBNM-TAXABLE-YTD > 50000.00
               MOVE 50000.00 TO SBNM-TAXABLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4460-DESCRIBE-BRANCH.
           EVALUATE SBNM-BRANCH-CD
               WHEN 'BD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAXABLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-TAXABLE.
           COMPUTE WS-WORK-AMT = SBNM-TAXABLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-TAXABLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE SBNM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE ACCRUAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4480-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN SBNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO SBNM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SBNM-PREFERRED-FLAG = 'Y'
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED = SBNM-ACCRUAL-AMT
                       * SBNM-MARGIN / 200
               WHEN SBNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO SBNM-PURCHASE-YTD
               WHEN SBNT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO SBNM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE SBNM-PURCHASE-YTD ROUNDED = SBNM-ACCRUAL-AMT
                       * SBNM-MARGIN / 100
           END-EVALUATE
           IF SBNM-PURCHASE-YTD > 0 AND SBNM-PURCHASE-YTD < 10.00
               MOVE 10.00 TO SBNM-PURCHASE-YTD
           END-IF
           IF SBNM-PURCHASE-YTD > 150.00
               MOVE 150.00 TO SBNM-PURCHASE-YTD
           END-IF
           ADD SBNM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4490-ACCRUE-FACE.
           IF SBNM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SBNM-FACE-YTD * SBNM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SBNM-ACCRUAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SERIES CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-SERIES.
           EVALUATE SBNM-SERIES-CD
               WHEN 'DD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E315 CONTACT REQUIRED
      * E341 REDEEM YTD NEGATIVE
      * E440 REDEEM YTD OVER 10000
      * E741 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SBNM-CONTACT = SPACES
                   MOVE 'E315' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD < 0
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN SBNM-REDEEM-YTD > 10000.00
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN SBNM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF SBNT-EFF-DATE > 0 AND SBNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SBNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SBNT-REDEEM-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-PAYMENT.
           IF SBNM-PAYMENT-CNT < 9999999
               ADD 1 TO SBNM-PAYMENT-CNT
           END-IF
           IF SBNM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO SBNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4540-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-AMT - (WS-WORK-AMT * 4)
           COMPUTE SBNM-PURCHASE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4550-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = SBNM-PURCHASE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SBNM-PURCHASE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE SBNM-TAXABLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SBLIM.
           INITIALIZE SBLIM-PARMS
           MOVE SBNM-ISSUE-PRICE-YTD
             TO SBLIM-ISSUE-PRICE-YTD
           MOVE SBNM-FACE-YTD
             TO SBLIM-FACE-YTD
           MOVE SBNM-PURCHASE-YTD
             TO SBLIM-PURCHASE-YTD
           MOVE SBNM-TAXABLE-AMT
             TO SBLIM-TAXABLE-AMT
           MOVE SBNM-PURCHASE-AMT
             TO SBLIM-PURCHASE-AMT
           MOVE SBNM-TAXABLE-YTD
             TO SBLIM-TAXABLE-YTD
           MOVE SBNM-LAST-MAINT-DATE
             TO SBLIM-LAST-MAINT-DATE
           MOVE SBNM-NOTICE-DATE
             TO SBLIM-NOTICE-DATE
           MOVE SBNM-MATURITY-DATE
             TO SBLIM-MATURITY-DATE
           MOVE SBNM-NEXT-REVIEW-DATE
             TO SBLIM-NEXT-REVIEW-DATE
           MOVE SBNM-SOURCE-CD
             TO SBLIM-SOURCE-CD
           MOVE SBNM-REASON-CD
             TO SBLIM-REASON-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBLIM-ISSUE-TYPE-CD
           MOVE SBNM-SERIES-CD
             TO SBLIM-SERIES-CD
           MOVE SBNM-OVERRIDE-FLAG
             TO SBLIM-OVERRIDE-FLAG
           MOVE SBNM-PREFERRED-FLAG
             TO SBLIM-PREFERRED-FLAG
           MOVE SBNM-HOLD-FLAG
             TO SBLIM-HOLD-FLAG
           MOVE SBNM-PAYMENT-CNT
             TO SBLIM-PAYMENT-CNT
           MOVE SBNM-EXCEPT-CNT
             TO SBLIM-EXCEPT-CNT
           MOVE SBNM-PENALTY-PCT
             TO SBLIM-PENALTY-PCT
           MOVE SBNM-FEE-PCT
             TO SBLIM-FEE-PCT
           MOVE SBNM-REF-NO
             TO SBLIM-REF-NO
           MOVE SBNM-ADDR
             TO SBLIM-ADDR
           CALL 'SBLIM' USING SBLIM-PARMS
           EVALUATE SBLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBLIM-PURCHASE-YTD
                     TO SBNM-PURCHASE-YTD
                   MOVE SBLIM-TAXABLE-AMT
                     TO SBNM-TAXABLE-AMT
                   MOVE SBLIM-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBLIM-TAXABLE-YTD
                     TO SBNM-TAXABLE-YTD
                   MOVE SBLIM-PREFERRED-FLAG
                     TO SBNM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SBLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-SBFEE.
           INITIALIZE SBFEE-PARMS
           MOVE SBNM-PURCHASE-AMT
             TO SBFEE-PURCHASE-AMT
           MOVE SBNM-FACE-YTD
             TO SBFEE-FACE-YTD
           MOVE SBNM-REDEEM-YTD
             TO SBFEE-REDEEM-YTD
           MOVE SBNM-ACCRUAL-AMT
             TO SBFEE-ACCRUAL-AMT
           MOVE SBNM-INTEREST-YTD
             TO SBFEE-INTEREST-YTD
           MOVE SBNM-TAXABLE-AMT
             TO SBFEE-TAXABLE-AMT
           MOVE SBNM-NOTICE-DATE
             TO SBFEE-NOTICE-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBFEE-NEXT-DUE-DATE
           MOVE SBNM-OPEN-DATE
             TO SBFEE-OPEN-DATE
           MOVE SBNM-LAST-MAINT-DATE
             TO SBFEE-LAST-MAINT-DATE
           MOVE SBNM-REASON-CD
             TO SBFEE-REASON-CD
           MOVE SBNM-STATUS-CD
             TO SBFEE-STATUS-CD
           MOVE SBNM-BRANCH-CD
             TO SBFEE-BRANCH-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBFEE-ISSUE-TYPE-CD
           MOVE SBNM-AUTO-RENEW-FLAG
             TO SBFEE-AUTO-RENEW-FLAG
           MOVE SBNM-NOTICE-SENT-FLAG
             TO SBFEE-NOTICE-SENT-FLAG
           MOVE SBNM-REVIEW-FLAG
             TO SBFEE-REVIEW-FLAG
           MOVE SBNM-RETRY-CNT
             TO SBFEE-RETRY-CNT
           MOVE SBNM-EXCEPT-CNT
             TO SBFEE-EXCEPT-CNT
           MOVE SBNM-MARGIN
             TO SBFEE-MARGIN
           MOVE SBNM-PENALTY-PCT
             TO SBFEE-PENALTY-PCT
           MOVE SBNM-CONTACT
             TO SBFEE-CONTACT
           MOVE SBNM-DESC
             TO SBFEE-DESC
           CALL 'SBFEE' USING SBFEE-PARMS
           EVALUATE SBFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBFEE-FACE-YTD
                     TO SBNM-FACE-YTD
                   MOVE SBFEE-ACCRUAL-AMT
                     TO SBNM-ACCRUAL-AMT
                   MOVE SBFEE-TAXABLE-AMT
                     TO SBNM-TAXABLE-AMT
                   MOVE SBFEE-AUTO-RENEW-FLAG
                     TO SBNM-AUTO-RENEW-FLAG
                   MOVE SBFEE-REVIEW-FLAG
                     TO SBNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-SBFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SBN FIELDS TO SBPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-SBPEN.
           INITIALIZE SBPEN-PARMS
           MOVE SBNM-ACCRUAL-YTD
             TO SBPEN-ACCRUAL-YTD
           MOVE SBNM-PURCHASE-YTD
             TO SBPEN-PURCHASE-YTD
           MOVE SBNM-PURCHASE-AMT
             TO SBPEN-PURCHASE-AMT
           MOVE SBNM-REDEEM-AMT
             TO SBPEN-REDEEM-AMT
           MOVE SBNM-ACCRUAL-AMT
             TO SBPEN-ACCRUAL-AMT
           MOVE SBNM-INTEREST-YTD
             TO SBPEN-INTEREST-YTD
           MOVE SBNM-NOTICE-DATE
             TO SBPEN-NOTICE-DATE
           MOVE SBNM-LAST-MAINT-DATE
             TO SBPEN-LAST-MAINT-DATE
           MOVE SBNM-NEXT-REVIEW-DATE
             TO SBPEN-NEXT-REVIEW-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBPEN-NEXT-DUE-DATE
           MOVE SBNM-STATUS-CD
             TO SBPEN-STATUS-CD
           MOVE SBNM-SERIES-CD
             TO SBPEN-SERIES-CD
           MOVE SBNM-BRANCH-CD
             TO SBPEN-BRANCH-CD
           MOVE SBNM-SOURCE-CD
             TO SBPEN-SOURCE-CD
           MOVE SBNM-PREFERRED-FLAG
             TO SBPEN-PREFERRED-FLAG
           MOVE SBNM-HOLD-FLAG
             TO SBPEN-HOLD-FLAG
           MOVE SBNM-OVERRIDE-FLAG
             TO SBPEN-OVERRIDE-FLAG
           MOVE SBNM-PAYMENT-CNT
             TO SBPEN-PAYMENT-CNT
           MOVE SBNM-CYCLE-CNT
             TO SBPEN-CYCLE-CNT
           MOVE SBNM-MARGIN
             TO SBPEN-MARGIN
           MOVE SBNM-FEE-PCT
             TO SBPEN-FEE-PCT
           MOVE SBNM-REF-NO
             TO SBPEN-REF-NO
           MOVE SBNM-ADDR
             TO SBPEN-ADDR
           CALL 'SBPEN' USING SBPEN-PARMS
           EVALUATE SBPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SBPEN-ACCRUAL-YTD
                     TO SBNM-ACCRUAL-YTD
                   MOVE SBPEN-PURCHASE-AMT
                     TO SBNM-PURCHASE-AMT
                   MOVE SBPEN-INTEREST-YTD
                     TO SBNM-INTEREST-YTD
                   MOVE SBPEN-HOLD-FLAG
                     TO SBNM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SBPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE SBNT-KEY TO RD-KEY
           MOVE SBNT-TRAN-CD TO RD-CODE
           MOVE SBNT-REDEEM-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE SBNT-KEY TO RD-KEY
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
           DISPLAY 'SBNRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SBNRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
