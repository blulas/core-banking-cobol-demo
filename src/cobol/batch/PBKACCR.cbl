      *================================================================*
      * PROGRAM  : PBKACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - DAILY ACCRUAL. READS THE PBK    *
      *            TRANSACTIONS, UPDATES THE PBK MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE PRINTING*
      *            AND UNPOSTED ITEM CONTROL                           *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBSTAT, PBDTE, PBPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-20 EWS  ORIGINAL                                       *
      * 1999-06-11 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-28 EWS  CARRYFWD STATUS CODES ADDED CHG8815            *
      * 2003-03-11 RJT  INTEREST EXCEPTIONS TO OPS QUEUE REQ1982       *
      * 2004-09-03 CRP  INTEREST LIMIT REVISED PRB7790                 *
      * 2005-11-14 GAF  LINE CALCULATION CORRECTED INC7089             *
      * 2006-08-25 EWS  PERFORMANCE - LINE READ AHEAD CHG9812          *
      * 2006-04-14 PQR  PRINTED NOTICE TEXT PER LEGAL INC7547          *
      * 2007-09-07 EWS  PRINTED TABLE EXPANDED REQ2598                 *
      * 2009-07-15 MKL  BALANCE EXCEPTIONS TO OPS QUEUE REQ2951        *
      * 2012-12-16 LMB  PRINTED TABLE EXPANDED REQ3342                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKACCR.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "PBKACRPT"
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
                                           VALUE 25.000000.
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
      * WS-PRINTED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000257000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000715000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000487000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000068500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000049500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000494000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000742000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000027500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009150000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000522000'.
       01  WS-PRINTED-BAND-TBL REDEFINES WS-PRINTED-BAND-VALUES.
           05  WS-PRINTED-BAND-ROW OCCURS 11 TIMES.
               10  WS-PRINTED-BAND-LOW     PIC 9(09)V99.
               10  WS-PRINTED-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CARRYFWD-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CARRYFWD-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000653500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000540000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000222000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000369000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004300000000753500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004400000000601000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009400000000746000'.
       01  WS-CARRYFWD-BAND-TBL REDEFINES WS-CARRYFWD-BAND-VALUES.
           05  WS-CARRYFWD-BAND-ROW OCCURS 8 TIMES.
               10  WS-CARRYFWD-BAND-LOW    PIC 9(09)V99.
               10  WS-CARRYFWD-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BALANCE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BALANCE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000265000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000790000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000227000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000022500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027850000000233500'.
       01  WS-BALANCE-GRID-TBL REDEFINES WS-BALANCE-GRID-VALUES.
           05  WS-BALANCE-GRID-ROW OCCURS 7 TIMES.
               10  WS-BALANCE-GRID-LOW     PIC 9(09)V99.
               10  WS-BALANCE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINTED-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000790000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000707500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000678500'.
       01  WS-PRINTED-BANDX-TBL REDEFINES WS-PRINTED-BANDX-VALUES.
           05  WS-PRINTED-BANDX-ROW OCCURS 4 TIMES.
               10  WS-PRINTED-BANDX-LOW    PIC 9(09)V99.
               10  WS-PRINTED-BANDX-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKACCR  PASSBOOK PRINTING'.
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
       COPY PBSTATP.
       COPY PBDTEP.
       COPY PBPRCP.

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
               PERFORM 3000-PRORATE-LINE
               PERFORM 3010-SET-CHANNEL
               PERFORM 3020-DESCRIBE-SOURCE
               PERFORM 3030-ASSESS-REVERSAL
               PERFORM 3040-FLAG-LINE
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-SPLIT-INTEREST
               PERFORM 3070-SET-PRINTER
               PERFORM 3080-SELECT-MARGIN
               PERFORM 3090-SPLIT-UNPOSTED
               PERFORM 3100-EDIT-REF-NO
               PERFORM 3110-DESCRIBE-CHANNEL
               PERFORM 3120-SET-BRANCH
               PERFORM 3130-ACCUM-CHANNEL
               PERFORM 3140-SPLIT-PRINTED
               PERFORM 3150-ASSESS-REVIEW
               PERFORM 3160-AGE-MATURITY
               PERFORM 3170-SPLIT-PRINTED
               PERFORM 3180-FLAG-INTEREST
               PERFORM 3190-DESCRIBE-SOURCE
               PERFORM 3200-COUNT-PAYMENT
               PERFORM 3210-AGE-NEXT-REVIEW
               PERFORM 3220-PRORATE-PRINTED
               PERFORM 3230-AGE-NEXT-DUE
               PERFORM 3240-EDIT-MEMO
               PERFORM 3250-ACCUM-REGION
               PERFORM 3260-COUNT-SEQ
               PERFORM 3270-FLAG-CARRYFWD
               PERFORM 3280-SET-BRANCH
               PERFORM 3290-AGE-POST
               PERFORM 3300-LIMIT-ITEM
               PERFORM 3310-FLAG-BALANCE
               PERFORM 3320-EDIT-DESC
               PERFORM 3330-COUNT-SEQ
               PERFORM 3340-SPLIT-BALANCE
               PERFORM 3350-COUNT-SEQ
               PERFORM 3360-COUNT-PAYMENT
               PERFORM 3370-CHECK-POST
               PERFORM 3380-COUNT-PAYMENT
               PERFORM 3390-LIMIT-ITEM
               PERFORM 3400-CHECK-LAST-ACT
               PERFORM 3410-DESCRIBE-TRAN
               PERFORM 3420-SPLIT-PRINTED
               PERFORM 3430-EDIT-CONTACT
               PERFORM 3440-SPLIT-CARRYFWD
               PERFORM 3450-ACCRUE-CARRYFWD
               PERFORM 3460-COUNT-RETRY
               PERFORM 3470-DESCRIBE-PRINTER
               PERFORM 3480-FLAG-UNPOSTED
               PERFORM 3490-DESCRIBE-CHANNEL
               PERFORM 3500-SELECT-PENALTY-PCT
               PERFORM 3510-PRORATE-PRINTED
               PERFORM 3520-EDIT-CUST-ID
               PERFORM 3530-ASSESS-HOLD
               PERFORM 3540-ACCUM-CHANNEL
               PERFORM 3550-AGE-NEXT-DUE
               PERFORM 3560-EDIT-NAME
               PERFORM 3570-FLAG-BALANCE
               PERFORM 3580-SELECT-MARGIN
               PERFORM 3590-CHECK-POST
               PERFORM 3600-FLAG-INTEREST
               PERFORM 3610-EDIT-ADDR
               PERFORM 3620-LIMIT-PRINTED
               PERFORM 3630-SET-REGION
               PERFORM 3640-FLAG-ITEM
               PERFORM 3650-PRORATE-CARRYFWD
               PERFORM 3660-AGE-LAST-ACT
               PERFORM 3670-SET-TRAN
               PERFORM 3680-SET-TRAN
               PERFORM 3690-PRORATE-CARRYFWD
               PERFORM 3700-FLAG-LINE
               PERFORM 3710-AGE-POST
               PERFORM 3720-FLAG-CARRYFWD
               PERFORM 3730-SPLIT-LINE
               PERFORM 3740-ACCRUE-UNPOSTED
               PERFORM 3750-SET-SOURCE
               PERFORM 3760-SET-TRAN
               PERFORM 3770-FLAG-BALANCE
               PERFORM 3780-LIMIT-PRINTED
               PERFORM 3790-COUNT-ITEM
               PERFORM 3800-CHECK-EXPIRY
               PERFORM 3810-ASSESS-SUSPEND
               PERFORM 3820-COUNT-ITEM
               PERFORM 3830-ACCUM-CHANNEL
               PERFORM 3840-SET-TRAN
               PERFORM 3850-SPLIT-LINE
               PERFORM 3860-AGE-EFFECTIVE
               PERFORM 3870-FLAG-INTEREST
               PERFORM 3880-LIMIT-CARRYFWD
               PERFORM 3890-SET-REGION
               PERFORM 3900-CHECK-EFF
               PERFORM 3910-FLAG-PRINTED
               PERFORM 3920-CHECK-MATURITY
               PERFORM 3930-FLAG-INTEREST
               PERFORM 3940-SPLIT-LINE
               PERFORM 3950-DESCRIBE-BOOK-TYPE
               PERFORM 3960-ASSESS-REVERSAL
               PERFORM 3970-SELECT-MARGIN
               PERFORM 7020-CALL-PBSTAT
               PERFORM 7010-CALL-PBDTE
               PERFORM 7000-CALL-PBPRC
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
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-CHANNEL.
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
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE BALANCE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBKM-BALANCE-YTD *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PBKM-BALANCE-YTD *
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
      * LINE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3040-FLAG-LINE.
           IF PBKM-LINE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKM-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN EXPIRY DATE IS PAST
      * EP -> EX WHEN EXPIRY DATE IS PAST
      * EX -> JE WHEN EXPIRY DATE IS PAST
      * JE -> MR WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED BANDX TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-BANDX-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-INTEREST-AMT NOT < WS-PRINTED-BANDX-LOW(WS-TX)
                   MOVE WS-PRINTED-BANDX-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-UNPOSTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E391 REF NO REQUIRED
      * E415 CARRYFWD AMT NEGATIVE
      * E281 CARRYFWD AMT OVER 99999
      * E738 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-REF-NO = SPACES
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT < 0
                   MOVE 'E415' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT > 99999.00
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E738' TO WS-REJECT-CODE
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
       3110-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN EFFECTIVE DATE IS PAST
      * NV -> PP WHEN EFFECTIVE DATE IS PAST
      * PP -> SH WHEN EFFECTIVE DATE IS PAST
      * SH -> ZP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-LINE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKT-CARRYFWD-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 10.00 TO PBKM-INTEREST-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 3.00 TO PBKM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-AMT ROUNDED = PBKT-CARRYFWD-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-AMT > 0 AND PBKM-INTEREST-AMT < 1.00
               MOVE 1.00 TO PBKM-INTEREST-AMT
           END-IF
           IF PBKM-INTEREST-AMT > 50.00
               MOVE 50.00 TO PBKM-INTEREST-AMT
           END-IF
           ADD PBKM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3180-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-NEXT-REVIEW.
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
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKM-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-NEXT-DUE.
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
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E747 MEMO REQUIRED
      * E043 PRINTED YTD NEGATIVE
      * E062 PRINTED YTD OVER 5000
      * E561 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD > 5000.00
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN PBKM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-REGION.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 5
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN LAST ACT DATE IS PAST
      * NV -> PP WHEN LAST ACT DATE IS PAST
      * PP -> SH WHEN LAST ACT DATE IS PAST
      * SH -> ZP WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-POST.
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
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3300-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E237 DESC REQUIRED
      * E145 BALANCE YTD NEGATIVE
      * E261 BALANCE YTD OVER 10000
      * E284 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD < 0
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN PBKM-BALANCE-YTD > 10000.00
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 5
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKM-LINE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 3
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-LAST-ACT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PBKM-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E985 CONTACT REQUIRED
      * E559 UNPOSTED YTD NEGATIVE
      * E863 UNPOSTED YTD OVER 1000000
      * E345 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CONTACT = SPACES
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD < 0
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD > 1000000.00
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKT-CARRYFWD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKT-CARRYFWD-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3480-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BALANCE GRID TIER FOR THE LINE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BALANCE-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PBKM-LINE-YTD NOT < WS-BALANCE-GRID-LOW(WS-TX)
                   MOVE WS-BALANCE-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E270 CUST ID REQUIRED
      * E880 CARRYFWD AMT NEGATIVE
      * E270 CARRYFWD AMT OVER 250000
      * E576 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT < 0
                   MOVE 'E880' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT > 250000.00
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED YTD FEE ON THE BALANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3530-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-YTD ROUNDED = PBKM-BALANCE-YTD *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-PRINTER-CD = 'CB'
                   MOVE 15.00 TO PBKM-PRINTED-YTD
               WHEN PBKM-PRINTER-CD = 'EP'
                   MOVE 20.00 TO PBKM-PRINTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-YTD ROUNDED = PBKM-BALANCE-YTD *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-PRINTED-YTD > 0 AND PBKM-PRINTED-YTD < 1.00
               MOVE 1.00 TO PBKM-PRINTED-YTD
           END-IF
           IF PBKM-PRINTED-YTD > 250.00
               MOVE 250.00 TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-NEXT-DUE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E329 NAME REQUIRED
      * E690 WORK AMT NEGATIVE
      * E706 WORK AMT OVER 25000
      * E027 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E329' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-BALANCE.
           IF PBKM-BALANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED BANDX TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-BANDX-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-INTEREST-YTD NOT < WS-PRINTED-BANDX-LOW(WS-TX)
                   MOVE WS-PRINTED-BANDX-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PBKT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 ADDR REQUIRED
      * E715 WORK AMT 2 NEGATIVE
      * E911 WORK AMT 2 OVER 5000
      * E215 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-ADDR = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-PRINTED.
           IF PBKT-PRINTED-AMT < 10.00
               MOVE 10.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-PRINTED-AMT > 50000.00
               MOVE 50000.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT DUE DATE IS PAST
      * GL -> PE WHEN NEXT DUE DATE IS PAST
      * PE -> RJ WHEN NEXT DUE DATE IS PAST
      * RJ -> VB WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3640-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-AMT ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-LAST-ACT.
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
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN NEXT REVIEW DATE IS PAST
      * PR -> RB WHEN NEXT REVIEW DATE IS PAST
      * RB -> LS WHEN NEXT REVIEW DATE IS PAST
      * LS -> CF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN EFF DATE IS PAST
      * PR -> RB WHEN EFF DATE IS PAST
      * RB -> LS WHEN EFF DATE IS PAST
      * LS -> CF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-CARRYFWD-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKT-CARRYFWD-AMT
           END-IF
           ADD PBKT-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LINE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3700-FLAG-LINE.
           IF PBKM-LINE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-POST.
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
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3720-FLAG-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKT-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-PRINTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN NEXT REVIEW DATE IS PAST
      * ZC -> ZY WHEN NEXT REVIEW DATE IS PAST
      * ZY -> JX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN POST DATE IS PAST
      * PR -> RB WHEN POST DATE IS PAST
      * RB -> LS WHEN POST DATE IS PAST
      * LS -> CF WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3770-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 25.00
               MOVE 25.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 10000.00
               MOVE 10000.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 12
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE LINE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3810-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKT-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 12.00 TO PBKT-LINE-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 3.00 TO PBKT-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKT-LINE-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKT-LINE-AMT > 0 AND PBKT-LINE-AMT < 5.00
               MOVE 5.00 TO PBKT-LINE-AMT
           END-IF
           IF PBKT-LINE-AMT > 25.00
               MOVE 25.00 TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 12
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN POST DATE IS PAST
      * PR -> RB WHEN POST DATE IS PAST
      * RB -> LS WHEN POST DATE IS PAST
      * LS -> CF WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKT-POST-DATE > 0 AND PBKT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LINE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-YTD - (WS-WORK-AMT * 3)
           COMPUTE PBKM-LINE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3870-FLAG-INTEREST.
           IF PBKM-INTEREST-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-CARRYFWD.
           IF PBKT-CARRYFWD-AMT < 25.00
               MOVE 25.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-CARRYFWD-AMT > 1000.00
               MOVE 1000.00 TO PBKT-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN CLOSE DATE IS PAST
      * GL -> PE WHEN CLOSE DATE IS PAST
      * PE -> RJ WHEN CLOSE DATE IS PAST
      * RJ -> VB WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-PRINTED.
           IF PBKM-PRINTED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-MATURITY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE BALANCE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3960-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKM-BALANCE-YTD
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO PBKM-UNPOSTED-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKM-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKM-BALANCE-YTD
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-YTD > 0 AND PBKM-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBKM-UNPOSTED-YTD
           END-IF
           IF PBKM-UNPOSTED-YTD > 35.00
               MOVE 35.00 TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CARRYFWD BAND TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CARRYFWD-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-UNPOSTED-YTD NOT < WS-CARRYFWD-BAND-LOW(WS-TX)
                   MOVE WS-CARRYFWD-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PBSTAT.
           INITIALIZE PBSTAT-PARMS
           MOVE PBKM-UNPOSTED-YTD
             TO PBSTAT-UNPOSTED-YTD
           MOVE PBKM-BALANCE-AMT
             TO PBSTAT-BALANCE-AMT
           MOVE PBKM-LINE-YTD
             TO PBSTAT-LINE-YTD
           MOVE PBKM-CARRYFWD-YTD
             TO PBSTAT-CARRYFWD-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBSTAT-UNPOSTED-AMT
           MOVE PBKM-LINE-AMT
             TO PBSTAT-LINE-AMT
           MOVE PBKM-CLOSE-DATE
             TO PBSTAT-CLOSE-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBSTAT-EFFECTIVE-DATE
           MOVE PBKM-NEXT-DUE-DATE
             TO PBSTAT-NEXT-DUE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBSTAT-NEXT-REVIEW-DATE
           MOVE PBKM-BOOK-TYPE-CD
             TO PBSTAT-BOOK-TYPE-CD
           MOVE PBKM-PRINTER-CD
             TO PBSTAT-PRINTER-CD
           MOVE PBKM-REGION-CD
             TO PBSTAT-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBSTAT-SOURCE-CD
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBSTAT-NOTICE-SENT-FLAG
           MOVE PBKM-REVIEW-FLAG
             TO PBSTAT-REVIEW-FLAG
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBSTAT-AUTO-RENEW-FLAG
           MOVE PBKM-RETRY-CNT
             TO PBSTAT-RETRY-CNT
           MOVE PBKM-CYCLE-CNT
             TO PBSTAT-CYCLE-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBSTAT-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBSTAT-MARGIN
           MOVE PBKM-CONTACT
             TO PBSTAT-CONTACT
           MOVE PBKM-CUST-ID
             TO PBSTAT-CUST-ID
           CALL 'PBSTAT' USING PBSTAT-PARMS
           EVALUATE PBSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBSTAT-BALANCE-AMT
                     TO PBKM-BALANCE-AMT
                   MOVE PBSTAT-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBSTAT-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBSTAT-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   MOVE PBSTAT-AUTO-RENEW-FLAG
                     TO PBKM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PBSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE PBDTE-PRINTED-YTD
                     TO PBKM-PRINTED-YTD
                   MOVE PBDTE-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBDTE-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBDTE-CARRYFWD-AMT
                     TO PBKM-CARRYFWD-AMT
                   MOVE PBDTE-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
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

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PBPRC.
           INITIALIZE PBPRC-PARMS
           MOVE PBKM-PRINTED-AMT
             TO PBPRC-PRINTED-AMT
           MOVE PBKM-LINE-YTD
             TO PBPRC-LINE-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBPRC-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBPRC-CARRYFWD-YTD
           MOVE PBKM-INTEREST-AMT
             TO PBPRC-INTEREST-AMT
           MOVE PBKM-INTEREST-YTD
             TO PBPRC-INTEREST-YTD
           MOVE PBKM-MATURITY-DATE
             TO PBPRC-MATURITY-DATE
           MOVE PBKM-EXPIRY-DATE
             TO PBPRC-EXPIRY-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBPRC-EFFECTIVE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBPRC-NEXT-REVIEW-DATE
           MOVE PBKM-CHANNEL-CD
             TO PBPRC-CHANNEL-CD
           MOVE PBKM-REGION-CD
             TO PBPRC-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBPRC-SOURCE-CD
           MOVE PBKM-PRINTER-CD
             TO PBPRC-PRINTER-CD
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBPRC-AUTO-RENEW-FLAG
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBPRC-NOTICE-SENT-FLAG
           MOVE PBKM-HOLD-FLAG
             TO PBPRC-HOLD-FLAG
           MOVE PBKM-PAYMENT-CNT
             TO PBPRC-PAYMENT-CNT
           MOVE PBKM-ITEM-CNT
             TO PBPRC-ITEM-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBPRC-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBPRC-MARGIN
           MOVE PBKM-CUST-ID
             TO PBPRC-CUST-ID
           MOVE PBKM-ADDR
             TO PBPRC-ADDR
           CALL 'PBPRC' USING PBPRC-PARMS
           EVALUATE PBPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBPRC-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBPRC-INTEREST-AMT
                     TO PBKM-INTEREST-AMT
                   MOVE PBPRC-AUTO-RENEW-FLAG
                     TO PBKM-AUTO-RENEW-FLAG
                   MOVE PBPRC-HOLD-FLAG
                     TO PBKM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PBPRC' TO ABND-PARAGRAPH
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
           DISPLAY 'PBKACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
