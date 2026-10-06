      *================================================================*
      * PROGRAM  : LNACCR                                              *
      * TYPE     : BATCH - EOD STEP 12                                 *
      * PURPOSE  : DAILY LOAN INTEREST ACCRUAL. FOR EVERY ACCRUING     *
      *            LOAN, ACCRUES INTEREST ON THE PRINCIPAL BALANCE FOR *
      *            EACH CALENDAR DAY SINCE THE LAST ACCRUAL DATE       *
      *            THROUGH THE BUSINESS DATE:                          *
      *              ACTUAL/365 (LPER) - RATE / DAYS IN THAT YEAR      *
      *                                  (365 OR 366) PER DAY          *
      *              ACTUAL/360 (LNTM, LLOC) - RATE / 360 PER DAY      *
      *              30/360 (LCRE) - 30/360 DAY COUNT, RATE / 360      *
      *            VARIABLE RATE LOANS ARE RESET FROM THE BANKCTL      *
      *            INDEX (PRIME OR SOFR) PLUS THE NOTE MARGIN, NOT     *
      *            BELOW THE NOTE FLOOR.                               *
      *            NO ACCRUAL ON NON-ACCRUAL, CHARGED-OFF OR PAID-OFF  *
      *            LOANS.                                              *
      * INPUT    : BANKCTL (VIA GETCTL)                                *
      * I-O      : LOANMST                                             *
      * OUTPUT   : LNACRPT  ACCRUAL REPORT                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - ACTUAL/360 COMMERCIAL ACCRUAL       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-08 MKL  RATE FLOOR ON VARIABLE NOTES                   *
      * 2003-05-09 BHS  30/360 FOR CRE (DATEUTIL D360)                 *
      * 2008-01-02 DPS  ACCRUE EACH CALENDAR DAY - WEEKEND/HOLIDAY     *
      *                 CATCH-UP WAS ONE LUMP AT TODAY'S RATE INC12007 *
      * 2016-04-25 AKW  ACTUAL/365 FOR LPER - LEAP YEAR VIA DIYR       *
      * 2022-01-10 AKW  SOFR INDEX - REG-2022-L01                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNACCR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO "LOANMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NUMBER
               ALTERNATE RECORD KEY IS LOAN-BORROWER-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-LOAN-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LNACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       COPY LOANREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-STATUS                  PIC X(02).
       01  WS-RPT-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-ACCRUED-COUNT            PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-RESET-COUNT              PIC 9(07) VALUE 0.
       01  WS-TOTALS.
           05  WS-TOT-ACCRUAL              PIC S9(11)V9(06) COMP-3
                                           VALUE 0.
       01  WS-ACCRUAL-WORK.
           05  WS-DAYS                     PIC S9(07) COMP.
           05  WS-DAY-INT                  PIC S9(09) COMP.
           05  WS-END-INT                  PIC S9(09) COMP.
           05  WS-DAY-DATE                 PIC 9(08).
           05  WS-DIYR                     PIC 9(03).
           05  WS-ACCRUAL                  PIC S9(09)V9(06) COMP-3.
           05  WS-DAY-ACCRUAL              PIC S9(09)V9(06) COMP-3.
           05  WS-NEW-RATE                 PIC 9(03)V9(06).
           05  WS-INDEX-RATE               PIC 9(03)V9(06).
           05  WS-OLD-RATE                 PIC 9(03)V9(06).
       01  WS-RATE-RESET-FLAG              PIC X(01).
           88  WS-RATE-WAS-RESET                     VALUE 'Y'.

       01  WS-HDR-1.
           05  FILLER PIC X(40) VALUE
               'LNACCR   CBS DAILY LOAN INTEREST ACCRUAL'.
           05  FILLER PIC X(16) VALUE '  BUSINESS DATE '.
           05  WS-HDR-DATE                 PIC 9(08).
       01  WS-HDR-2.
           05  FILLER PIC X(45) VALUE
               'LOAN NUMBER  PROD ST BASIS   DAYS  PRINCIPAL'.
           05  FILLER PIC X(21) VALUE
               ' BALANCE       RATE'.
           05  FILLER PIC X(50) VALUE
               '     ACCRUAL TODAY   ACCRUED TO DATE NOTE'.
       01  WS-DTL.
           05  WS-D-LOAN                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-PROD                   PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-STATUS                 PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  WS-D-BASIS                  PIC X(07).
           05  WS-D-DAYS                   PIC ZZZ9.
           05  WS-D-PRIN                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-D-RATE                   PIC ZZ9.999999.
           05  WS-D-ACCRUAL                PIC ZZZ,ZZZ,ZZ9.999999-.
           05  WS-D-ACCRUED                PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NOTE                   PIC X(40).
       01  WS-TRL.
           05  FILLER PIC X(14) VALUE 'LNACCR TOTALS '.
           05  FILLER PIC X(06) VALUE 'READ='.
           05  WS-T-READ                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(10) VALUE ' ACCRUED='.
           05  WS-T-ACCRUED                PIC ZZZ,ZZ9.
           05  FILLER PIC X(10) VALUE ' SKIPPED='.
           05  WS-T-SKIP                   PIC ZZZ,ZZ9.
           05  FILLER PIC X(13) VALUE ' RATE RESETS='.
           05  WS-T-RESET                  PIC ZZZ,ZZ9.
           05  FILLER PIC X(15) VALUE ' TOTAL ACCRUAL='.
           05  WS-T-TOTAL                  PIC ZZZ,ZZZ,ZZ9.99-.

       COPY BANKCTL.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM UNTIL WS-EOF
               READ LOAN-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2000-PROCESS-LOAN
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN I-O LOAN-FILE
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'LOANMST' TO ABND-FILE-NAME
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - LOAN MASTER' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'LNACRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - ACCRUAL REPORT' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-HDR-DATE
           WRITE REPORT-LINE FROM WS-HDR-1
           WRITE REPORT-LINE FROM WS-HDR-2.

      *================================================================*
       2000-PROCESS-LOAN.
           ADD 1 TO WS-READ-COUNT
           MOVE SPACES TO WS-D-NOTE
           MOVE ZERO TO WS-ACCRUAL WS-DAYS
           MOVE 'N' TO WS-RATE-RESET-FLAG

           EVALUATE TRUE
               WHEN LOAN-NON-ACCRUAL
                   MOVE 'NON-ACCRUAL - NO INTEREST ACCRUED'
                     TO WS-D-NOTE
                   PERFORM 2900-SKIP-LOAN
               WHEN LOAN-CHARGED-OFF
                   MOVE 'CHARGED OFF - NO INTEREST ACCRUED'
                     TO WS-D-NOTE
                   PERFORM 2900-SKIP-LOAN
               WHEN LOAN-PAID-OFF
                   PERFORM 2900-SKIP-LOAN
               WHEN LOAN-LAST-ACCRUAL-DATE NOT < CTL-BUSINESS-DATE
                   MOVE 'ALREADY ACCRUED THROUGH BUSINESS DATE'
                     TO WS-D-NOTE
                   PERFORM 2900-SKIP-LOAN
               WHEN OTHER
                   IF LOAN-VARIABLE-RATE
                       PERFORM 2100-RESET-VARIABLE-RATE
                   END-IF
                   PERFORM 2200-ACCRUE
                   PERFORM 2800-UPDATE-LOAN
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VARIABLE RATE = INDEX + MARGIN, NOT BELOW THE NOTE FLOOR       *
      *----------------------------------------------------------------*
       2100-RESET-VARIABLE-RATE.
           EVALUATE TRUE
               WHEN LOAN-INDEX-PRIME
                   MOVE CTL-PRIME-RATE TO WS-INDEX-RATE
               WHEN LOAN-INDEX-SOFR
                   MOVE CTL-SOFR-RATE  TO WS-INDEX-RATE
               WHEN OTHER
      *            NO INDEX ON FILE - LEAVE RATE AS BOOKED
                   EXIT PARAGRAPH
           END-EVALUATE
           COMPUTE WS-NEW-RATE = WS-INDEX-RATE + LOAN-MARGIN-RATE
           IF WS-NEW-RATE < LOAN-FLOOR-RATE
               MOVE LOAN-FLOOR-RATE TO WS-NEW-RATE
           END-IF
           IF WS-NEW-RATE NOT = LOAN-INT-RATE
               MOVE LOAN-INT-RATE TO WS-OLD-RATE
               MOVE WS-NEW-RATE TO LOAN-INT-RATE
               MOVE CTL-BUSINESS-DATE TO LOAN-LAST-RATE-CHG-DATE
               SET WS-RATE-WAS-RESET TO TRUE
               ADD 1 TO WS-RESET-COUNT
               MOVE WS-OLD-RATE TO WS-D-RATE
               STRING 'RATE RESET FROM ' DELIMITED BY SIZE
                      WS-D-RATE DELIMITED BY SIZE
                 INTO WS-D-NOTE
               END-STRING
           END-IF.

      *----------------------------------------------------------------*
      * ACCRUE FOR EACH CALENDAR DAY (LAST ACCRUAL DATE, BUS DATE]     *
      *----------------------------------------------------------------*
       2200-ACCRUE.
           EVALUATE TRUE
               WHEN LOAN-ACTUAL-365
                   MOVE 'ACT/365' TO WS-D-BASIS
                   PERFORM 2210-ACCRUE-ACTUAL-365
               WHEN LOAN-ACTUAL-360
                   MOVE 'ACT/360' TO WS-D-BASIS
                   PERFORM 2220-ACCRUE-ACTUAL-360
               WHEN LOAN-30-360
                   MOVE '30/360 ' TO WS-D-BASIS
                   PERFORM 2230-ACCRUE-30-360
               WHEN OTHER
                   MOVE 'ACT/360' TO WS-D-BASIS
                   PERFORM 2220-ACCRUE-ACTUAL-360
           END-EVALUATE.

       2210-ACCRUE-ACTUAL-365.
           COMPUTE WS-DAY-INT =
               FUNCTION INTEGER-OF-DATE(LOAN-LAST-ACCRUAL-DATE) + 1
           COMPUTE WS-END-INT =
               FUNCTION INTEGER-OF-DATE(CTL-BUSINESS-DATE)
           PERFORM UNTIL WS-DAY-INT > WS-END-INT
               COMPUTE WS-DAY-DATE =
                   FUNCTION DATE-OF-INTEGER(WS-DAY-INT)
               INITIALIZE DATE-PARMS
               MOVE 'DIYR' TO DTU-FUNCTION
               MOVE WS-DAY-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DIYR
               ELSE
                   MOVE 365 TO WS-DIYR
               END-IF
               COMPUTE WS-DAY-ACCRUAL ROUNDED =
                   LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100 / WS-DIYR
               ADD WS-DAY-ACCRUAL TO WS-ACCRUAL
               ADD 1 TO WS-DAYS
               ADD 1 TO WS-DAY-INT
           END-PERFORM.

       2220-ACCRUE-ACTUAL-360.
           INITIALIZE DATE-PARMS
           MOVE 'DIFF' TO DTU-FUNCTION
           MOVE LOAN-LAST-ACCRUAL-DATE TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE      TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS
           PERFORM WS-DAYS TIMES
               COMPUTE WS-DAY-ACCRUAL ROUNDED =
                   LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100 / 360
               ADD WS-DAY-ACCRUAL TO WS-ACCRUAL
           END-PERFORM.

       2230-ACCRUE-30-360.
           INITIALIZE DATE-PARMS
           MOVE 'D360' TO DTU-FUNCTION
           MOVE LOAN-LAST-ACCRUAL-DATE TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE      TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS
           IF WS-DAYS < ZERO
               MOVE ZERO TO WS-DAYS
           END-IF
           COMPUTE WS-ACCRUAL ROUNDED =
               LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100 * WS-DAYS / 360.

      *----------------------------------------------------------------*
       2800-UPDATE-LOAN.
           ADD WS-ACCRUAL TO LOAN-ACCRUED-INT
           COMPUTE LOAN-INT-ACCRUED-YTD ROUNDED =
               LOAN-INT-ACCRUED-YTD + WS-ACCRUAL
           MOVE CTL-BUSINESS-DATE TO LOAN-LAST-ACCRUAL-DATE
                                     LOAN-LAST-MAINT-DATE
           MOVE 'LNACCR' TO LOAN-LAST-MAINT-USER
           REWRITE LOAN-RECORD
           IF WS-LOAN-STATUS NOT = '00'
               MOVE 'LOANMST' TO ABND-FILE-NAME
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED - LOAN MASTER' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ACCRUED-COUNT
           ADD WS-ACCRUAL TO WS-TOT-ACCRUAL
           PERFORM 8000-WRITE-DETAIL.

       2900-SKIP-LOAN.
           ADD 1 TO WS-SKIP-COUNT
           MOVE SPACES TO WS-D-BASIS
           IF WS-D-NOTE NOT = SPACES
               PERFORM 8000-WRITE-DETAIL
           END-IF.

      *----------------------------------------------------------------*
       8000-WRITE-DETAIL.
           MOVE LOAN-NUMBER        TO WS-D-LOAN
           MOVE LOAN-PRODUCT-CODE  TO WS-D-PROD
           MOVE LOAN-STATUS        TO WS-D-STATUS
           MOVE WS-DAYS            TO WS-D-DAYS
           MOVE LOAN-PRINCIPAL-BAL TO WS-D-PRIN
           MOVE LOAN-INT-RATE      TO WS-D-RATE
           MOVE WS-ACCRUAL         TO WS-D-ACCRUAL
           MOVE LOAN-ACCRUED-INT   TO WS-D-ACCRUED
           WRITE REPORT-LINE FROM WS-DTL.

       9000-TERMINATE.
           MOVE WS-READ-COUNT    TO WS-T-READ
           MOVE WS-ACCRUED-COUNT TO WS-T-ACCRUED
           MOVE WS-SKIP-COUNT    TO WS-T-SKIP
           MOVE WS-RESET-COUNT   TO WS-T-RESET
           MOVE WS-TOT-ACCRUAL   TO WS-T-TOTAL
           WRITE REPORT-LINE FROM WS-TRL
           CLOSE LOAN-FILE REPORT-FILE
           DISPLAY 'LNACCR  READ=' WS-READ-COUNT
                   ' ACCRUED=' WS-ACCRUED-COUNT
                   ' SKIPPED=' WS-SKIP-COUNT
                   ' RATE-RESETS=' WS-RESET-COUNT
           MOVE 0 TO RETURN-CODE.

       9900-ABEND.
           MOVE 'LNACCR' TO ABND-PROGRAM
           MOVE '2000-PROCESS-LOAN' TO ABND-PARAGRAPH
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
