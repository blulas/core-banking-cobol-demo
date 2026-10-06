      *================================================================*
      * PROGRAM  : LNPAYOF                                             *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANSACTION : CB41                                             *
      * MAPSET   : LNPAYQS   MAP LNPAYQM                               *
      * PURPOSE  : LOAN PAYOFF QUOTE TO A GOOD-THROUGH DATE.           *
      *            ACCRUING LOANS (STATUS A / D / F):                  *
      *              PRINCIPAL BALANCE                                 *
      *            + ACCRUED INTEREST (THROUGH LAST ACCRUAL DATE)      *
      *            + PER DIEM X DAYS LAST ACCRUAL -> GOOD-THROUGH      *
      *            + LATE FEES OUTSTANDING                             *
      *            + PREPAYMENT PENALTY (LNTM ONLY)                    *
      *            - SUSPENSE BALANCE (UNAPPLIED FUNDS)                *
      *            NON-ACCRUAL LOANS (STATUS N):                       *
      *              PRINCIPAL + LATE FEES + NON-ACCRUAL INTEREST MEMO *
      *            PAID OFF (P) AND CHARGED OFF (C) LOANS ARE REJECTED.*
      *            GOOD-THROUGH DEFAULTS TO BUSINESS DATE + 10 DAYS    *
      *            AND MUST BE WITHIN 30 DAYS OF THE BUSINESS DATE.    *
      * FILES    : LOANMST  (KSDS)  READ                               *
      *            CUSTMST  (KSDS)  READ                               *
      * CALLS    : GETCTL   (BANKCTL)  - BUSINESS DATE                 *
      *            DATEUTIL (DATEPARM) - ADDD / DIFF / VALD / ADDM     *
      * XCTL     : CBMENU   (PF3)                                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2009-03-02 DPS  ORIGINAL - REPLACES PAPER PAYOFF WORKSHEET     *
      *                 LN-FRM-07                                      *
      * 2009-11-16 DPS  GOOD-THROUGH LIMITED TO 30 DAYS - INC10077     *
      * 2010-05-24 DPS  SUSPENSE BALANCE DEDUCTED - INC10295           *
      * 2012-04-30 DPS  NON-ACCRUAL PAYOFF USES INTEREST MEMO -        *
      *                 CHG1187                                        *
      * 2013-08-19 BHS  LNTM PREPAYMENT PENALTY 3/2/1 BY LOAN YEAR -   *
      *                 CHG1302                                        *
      * 2016-04-25 AKW  LPER - NO PREPAYMENT PENALTY (CONSUMER)        *
      * 2018-11-05 AKW  RESP/RESP2 ON ALL CICS COMMANDS - AUDIT A18-07 *
      * 2020-04-06 AKW  FORBEARANCE LOANS QUOTED AS ACCRUING           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNPAYOF.
       AUTHOR. D P SANDOVAL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNPAYOF'.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB41'.

       01  WS-CICS-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-RESP-D                   PIC 9(04).
           05  WS-RESP2-D                  PIC 9(04).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-DISP-DATE                PIC X(10).

       01  WS-FLAGS.
           05  WS-ERROR-FLAG               PIC X(01) VALUE 'N'.
               88  WS-ERROR                          VALUE 'Y'.
               88  WS-NO-ERROR                       VALUE 'N'.

       01  WS-MESSAGE                      PIC X(79) VALUE SPACES.
       01  WS-LOAN-KEY                     PIC X(12).
       01  WS-CUST-KEY                     PIC X(10).
       01  WS-BORROWER-NAME                PIC X(30).

      *    COMMAREA - SAVED BETWEEN PSEUDO-CONVERSATIONAL TASKS
       01  WS-COMMAREA.
           05  CA-PROGRAM                  PIC X(08).
           05  CA-LOAN-NUMBER              PIC X(12).
           05  CA-GOOD-THRU-DATE           PIC 9(08).
           05  FILLER                      PIC X(12).

      *----------------------------------------------------------------*
      * GOOD-THROUGH DATE RULES                                        *
      *----------------------------------------------------------------*
       01  WS-GOOD-THRU-RULES.
           05  WS-GT-DEFAULT-DAYS          PIC S9(05) VALUE +10.
           05  WS-GT-MAX-DAYS              PIC S9(05) VALUE +30.

      *----------------------------------------------------------------*
      * PREPAYMENT PENALTY - COMMERCIAL TERM (LNTM) ONLY. PERCENT OF   *
      * PRINCIPAL BALANCE BY LOAN YEAR AT THE GOOD-THROUGH DATE.       *
      *   LOAN YEAR 1  3.00%    LOAN YEAR 2  2.00%    LOAN YEAR 3 1.00%*
      *   AFTER THE THIRD ANNIVERSARY - NONE                           *
      *----------------------------------------------------------------*
       01  WS-PPP-TABLE-VALUES.
           05  FILLER                      PIC 9(01)V99 VALUE 3.00.
           05  FILLER                      PIC 9(01)V99 VALUE 2.00.
           05  FILLER                      PIC 9(01)V99 VALUE 1.00.
       01  WS-PPP-TABLE REDEFINES WS-PPP-TABLE-VALUES.
           05  WS-PPP-PCT                  PIC 9(01)V99 OCCURS 3 TIMES.

       01  WS-DATES.
           05  WS-BUS-DATE                 PIC 9(08).
           05  WS-GT-DATE                  PIC 9(08).
           05  WS-GT-MAX-DATE              PIC 9(08).
           05  WS-ACCRUAL-FROM-DATE        PIC 9(08).
           05  WS-ANNIV-1                  PIC 9(08).
           05  WS-ANNIV-2                  PIC 9(08).
           05  WS-ANNIV-3                  PIC 9(08).
           05  WS-GT-INPUT                 PIC X(08).
           05  WS-GT-INPUT-N REDEFINES WS-GT-INPUT
                                           PIC 9(08).

      *    PAYOFF COMPONENTS
       01  WS-PAYOFF-WORK.
           05  WS-PRINCIPAL                PIC S9(13)V99 COMP-3.
           05  WS-ACCRUED-INT              PIC S9(13)V99 COMP-3.
           05  WS-PER-DIEM                 PIC S9(09)V99 COMP-3.
           05  WS-INT-DAYS                 PIC S9(05)    COMP-3.
           05  WS-INT-TO-GT                PIC S9(13)V99 COMP-3.
           05  WS-NONACCR-INT              PIC S9(13)V99 COMP-3.
           05  WS-LATE-FEES                PIC S9(13)V99 COMP-3.
           05  WS-PREPAY-PENALTY           PIC S9(13)V99 COMP-3.
           05  WS-SUSPENSE                 PIC S9(13)V99 COMP-3.
           05  WS-PAYOFF-TOTAL             PIC S9(13)V99 COMP-3.
           05  WS-PPP-PCT-USED             PIC 9(01)V99.
           05  WS-LOAN-YEAR                PIC 9(01).
           05  WS-PP-DESC                  PIC X(40).

      *    EDITED DISPLAY FIELDS
       01  WS-EDIT-FIELDS.
           05  WS-AMT18-ED                 PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-PERDIEM-ED               PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-DAYS-ED                  PIC ZZZ9-.
           05  WS-PCT-ED                   PIC 9.99.
           05  WS-RATE-DISP.
               10  WS-RATE-ED              PIC ZZ9.9999.
               10  FILLER                  PIC X(01) VALUE '%'.

      *    DATE FORMAT WORK  CCYYMMDD -> MM/DD/CCYY
       01  WS-DATE-WORK.
           05  WS-FMT-DATE-IN              PIC 9(08).
           05  WS-FMT-DATE-R REDEFINES WS-FMT-DATE-IN.
               10  WS-FMT-CCYY             PIC 9(04).
               10  WS-FMT-MM               PIC 9(02).
               10  WS-FMT-DD               PIC 9(02).
           05  WS-FMT-DATE-OUT             PIC X(10).

      *    COPYBOOKS
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY LNPAYQS.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY LOANREC.
       COPY CUSTREC.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(40).

       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           PERFORM 1100-GET-BUSINESS-DATE

           IF EIBCALEN = 0
               PERFORM 1000-FIRST-TIME
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       PERFORM 9100-RETURN-TO-MENU
                   WHEN DFHCLEAR
                   WHEN DFHPF12
                       PERFORM 1000-FIRST-TIME
                   WHEN DFHENTER
                       PERFORM 2000-PROCESS-QUOTE
                   WHEN OTHER
                       PERFORM 2050-RECEIVE-MAP
                       MOVE 'INVALID KEY - USE ENTER, PF3 OR PF12'
                           TO WS-MESSAGE
                       PERFORM 8900-SEND-MAP
               END-EVALUATE
           END-IF

           PERFORM 9000-RETURN-TRANSID
           GOBACK.

      *================================================================*
      * 1000 - FIRST ENTRY OR CLEAR                                    *
      *================================================================*
       1000-FIRST-TIME.
           MOVE LOW-VALUES TO LNPAYQMO
           INITIALIZE WS-COMMAREA
           PERFORM 1200-DEFAULT-GOOD-THRU
           MOVE WS-GT-DATE TO GTDATEO CA-GOOD-THRU-DATE
           MOVE 'ENTER LOAN NUMBER - GOOD-THROUGH DEFAULTS TO 10 DAYS'
               TO WS-MESSAGE
           MOVE -1 TO LOANNOL
           PERFORM 8900-SEND-MAP.

      *    BUSINESS DATE FROM THE BANK CONTROL RECORD, NOT THE CLOCK
       1100-GET-BUSINESS-DATE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE CTL-BUSINESS-DATE TO WS-BUS-DATE.

      *    DEFAULT GOOD-THROUGH = BUSINESS DATE + 10 CALENDAR DAYS
       1200-DEFAULT-GOOD-THRU.
           INITIALIZE DATE-PARMS
           MOVE 'ADDD'             TO DTU-FUNCTION
           MOVE WS-BUS-DATE        TO DTU-DATE-1
           MOVE WS-GT-DEFAULT-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE    TO WS-GT-DATE.

      *================================================================*
      * 2000 - ENTER PRESSED - EDIT, READ LOAN, CALCULATE PAYOFF       *
      *================================================================*
       2000-PROCESS-QUOTE.
           SET WS-NO-ERROR TO TRUE
           PERFORM 2050-RECEIVE-MAP

           IF WS-NO-ERROR
               PERFORM 2100-EDIT-INPUT
           END-IF
           IF WS-NO-ERROR
               PERFORM 2200-READ-LOAN
           END-IF
           IF WS-NO-ERROR
               PERFORM 2250-CHECK-LOAN-STATUS
           END-IF
           IF WS-NO-ERROR
               PERFORM 2300-READ-BORROWER
               EVALUATE TRUE
                   WHEN LOAN-NON-ACCRUAL
                       PERFORM 2450-NONACCRUAL-PAYOFF
                   WHEN OTHER
                       PERFORM 2400-ACCRUING-PAYOFF
               END-EVALUATE
               PERFORM 2500-FORMAT-QUOTE
               MOVE LOAN-NUMBER TO CA-LOAN-NUMBER
               MOVE WS-GT-DATE  TO CA-GOOD-THRU-DATE
           END-IF

           PERFORM 8900-SEND-MAP.

       2050-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('LNPAYQM')
                     MAPSET('LNPAYQS')
                     INTO(LNPAYQMI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE LOW-VALUES TO LNPAYQMI
                   SET WS-ERROR TO TRUE
                   MOVE 'ENTER LOAN NUMBER' TO WS-MESSAGE
                   MOVE -1 TO LOANNOL
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE

           INSPECT LOANNOI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT GTDATEI REPLACING ALL LOW-VALUE BY SPACE.

      *----------------------------------------------------------------*
      * 2100 - SCREEN EDITS                                            *
      *----------------------------------------------------------------*
       2100-EDIT-INPUT.
           EVALUATE TRUE
               WHEN LOANNOI = SPACES
                   SET WS-ERROR TO TRUE
                   MOVE 'LOAN NUMBER IS REQUIRED' TO WS-MESSAGE
                   MOVE -1 TO LOANNOL
               WHEN LOANNOI IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'LOAN NUMBER MUST BE 12 DIGITS' TO WS-MESSAGE
                   MOVE DFHUNIMD TO LOANNOA
                   MOVE -1 TO LOANNOL
               WHEN OTHER
                   MOVE LOANNOI TO WS-LOAN-KEY
           END-EVALUATE

           IF WS-NO-ERROR
               PERFORM 2110-EDIT-GOOD-THRU
           END-IF.

      *----------------------------------------------------------------*
      * 2110 - GOOD-THROUGH DATE                                       *
      *   BLANK          -> BUSINESS DATE + 10 CALENDAR DAYS           *
      *   MUST BE A VALID CCYYMMDD DATE                                *
      *   MUST NOT BE BEFORE THE BUSINESS DATE                         *
      *   MUST NOT BE MORE THAN 30 DAYS AFTER THE BUSINESS DATE        *
      *----------------------------------------------------------------*
       2110-EDIT-GOOD-THRU.
           MOVE GTDATEI TO WS-GT-INPUT
           IF WS-GT-INPUT = SPACES OR WS-GT-INPUT = ZEROS
               PERFORM 1200-DEFAULT-GOOD-THRU
               MOVE WS-GT-DATE TO GTDATEO
           ELSE
               IF WS-GT-INPUT IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'GOOD-THROUGH DATE MUST BE CCYYMMDD'
                       TO WS-MESSAGE
               ELSE
                   MOVE WS-GT-INPUT-N TO WS-GT-DATE
                   INITIALIZE DATE-PARMS
                   MOVE 'VALD'        TO DTU-FUNCTION
                   MOVE WS-GT-DATE    TO DTU-DATE-1
                   CALL 'DATEUTIL' USING DATE-PARMS
                   IF NOT DTU-RESULT-YES
                       SET WS-ERROR TO TRUE
                       MOVE 'GOOD-THROUGH DATE IS NOT A VALID DATE'
                           TO WS-MESSAGE
                   END-IF
               END-IF
           END-IF

           IF WS-NO-ERROR
               INITIALIZE DATE-PARMS
               MOVE 'ADDD'         TO DTU-FUNCTION
               MOVE WS-BUS-DATE    TO DTU-DATE-1
               MOVE WS-GT-MAX-DAYS TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-GT-MAX-DATE
               EVALUATE TRUE
                   WHEN WS-GT-DATE < WS-BUS-DATE
                       SET WS-ERROR TO TRUE
                       MOVE 'GOOD-THROUGH IS BEFORE THE BUSINESS DATE'
                           TO WS-MESSAGE
                   WHEN WS-GT-DATE > WS-GT-MAX-DATE
                       SET WS-ERROR TO TRUE
                       MOVE 'GOOD-THROUGH MORE THAN 30 DAYS OUT'
                           TO WS-MESSAGE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF

           IF WS-ERROR
               MOVE DFHUNIMD TO GTDATEA
               MOVE -1 TO GTDATEL
           END-IF.

      *----------------------------------------------------------------*
      * 2200 - READ LOAN MASTER                                        *
      *----------------------------------------------------------------*
       2200-READ-LOAN.
           EXEC CICS READ FILE('LOANMST')
                     INTO(LOAN-RECORD)
                     RIDFLD(WS-LOAN-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET WS-ERROR TO TRUE
                   MOVE 'LOAN NOT FOUND' TO WS-MESSAGE
                   MOVE -1 TO LOANNOL
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2250 - LOANS THAT CANNOT BE QUOTED                             *
      *----------------------------------------------------------------*
       2250-CHECK-LOAN-STATUS.
           EVALUATE TRUE
               WHEN LOAN-PAID-OFF
                   SET WS-ERROR TO TRUE
                   MOVE 'LOAN ALREADY PAID OFF' TO WS-MESSAGE
               WHEN LOAN-CHARGED-OFF
                   SET WS-ERROR TO TRUE
                   MOVE 'CHARGED OFF - REFER TO RECOVERY'
                       TO WS-MESSAGE
               WHEN LOAN-IS-OPEN
                   CONTINUE
               WHEN OTHER
                   SET WS-ERROR TO TRUE
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'LOAN STATUS ' DELIMITED BY SIZE
                          LOAN-STATUS    DELIMITED BY SIZE
                          ' - PAYOFF QUOTE NOT AVAILABLE'
                                         DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
           END-EVALUATE
           IF WS-ERROR
               MOVE -1 TO LOANNOL
           END-IF.

       2300-READ-BORROWER.
           MOVE LOAN-BORROWER-CUST-ID TO WS-CUST-KEY
           MOVE SPACES TO WS-BORROWER-NAME
           EXEC CICS READ FILE('CUSTMST')
                     INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-CUST-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF CUST-INDIVIDUAL
                       STRING CUST-FIRST-NAME DELIMITED BY '  '
                              ' '             DELIMITED BY SIZE
                              CUST-LAST-NAME  DELIMITED BY '  '
                         INTO WS-BORROWER-NAME
                       END-STRING
                   ELSE
                       MOVE CUST-LAST-NAME TO WS-BORROWER-NAME
                   END-IF
               WHEN OTHER
                   MOVE '*** CIF RECORD NOT FOUND ***'
                       TO WS-BORROWER-NAME
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2400 - ACCRUING LOAN (STATUS A / D / F)                        *
      *----------------------------------------------------------------*
       2400-ACCRUING-PAYOFF.
           MOVE LOAN-PRINCIPAL-BAL   TO WS-PRINCIPAL
      *    ACCRUED INTEREST IS CARRIED AT 6 DECIMALS - ROUND TO CENTS
           COMPUTE WS-ACCRUED-INT ROUNDED = LOAN-ACCRUED-INT
           MOVE LOAN-LATE-FEES-OUTST TO WS-LATE-FEES
           MOVE LOAN-SUSPENSE-BAL    TO WS-SUSPENSE
           MOVE ZERO                 TO WS-NONACCR-INT

           PERFORM 2410-PER-DIEM
           PERFORM 2420-INTEREST-DAYS
           COMPUTE WS-INT-TO-GT ROUNDED = WS-PER-DIEM * WS-INT-DAYS
           PERFORM 2430-LOAN-YEAR
           PERFORM 2440-PREPAYMENT-PENALTY

           COMPUTE WS-PAYOFF-TOTAL = WS-PRINCIPAL
                                   + WS-ACCRUED-INT
                                   + WS-INT-TO-GT
                                   + WS-LATE-FEES
                                   + WS-PREPAY-PENALTY
                                   - WS-SUSPENSE

           EVALUATE TRUE
               WHEN WS-PAYOFF-TOTAL < 0
                   MOVE 'SUSPENSE EXCEEDS PAYOFF - REFER TO LOAN OPS'
                       TO WS-MESSAGE
               WHEN LOAN-FORBEARANCE
                   MOVE 'FORBEARANCE - INTEREST CONTINUES TO ACCRUE'
                       TO WS-MESSAGE
               WHEN LOAN-DELINQUENT
                   MOVE 'DELINQUENT - PAST DUE AMOUNTS ARE IN PAYOFF'
                       TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'PAYOFF QUOTE CALCULATED' TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2410 - PER DIEM INTEREST                                       *
      *----------------------------------------------------------------*
       2410-PER-DIEM.
      * PER DIEM - BANK STANDARD 360 DAY YEAR
           COMPUTE WS-PER-DIEM ROUNDED =
                   LOAN-PRINCIPAL-BAL * LOAN-INT-RATE / 100 / 360.
      * 2009-03 DPS PER DIEM FROM DAILY ACCRUAL FILE - REMOVED, FILE
      *             IS ONE DAY BEHIND DURING ONLINE DAY. SEE INC10077
      *    MOVE LNACC-DAILY-AMT TO WS-PER-DIEM

      *----------------------------------------------------------------*
      * 2420 - DAYS FROM LAST ACCRUAL DATE TO GOOD-THROUGH DATE        *
      *        (LOAN-ACCRUED-INT ALREADY COVERS THROUGH LAST ACCRUAL)  *
      *----------------------------------------------------------------*
       2420-INTEREST-DAYS.
           IF LOAN-LAST-ACCRUAL-DATE = ZERO
               MOVE LOAN-ORIG-DATE TO WS-ACCRUAL-FROM-DATE
           ELSE
               MOVE LOAN-LAST-ACCRUAL-DATE TO WS-ACCRUAL-FROM-DATE
           END-IF

           INITIALIZE DATE-PARMS
           MOVE 'DIFF'               TO DTU-FUNCTION
           MOVE WS-ACCRUAL-FROM-DATE TO DTU-DATE-1
           MOVE WS-GT-DATE           TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS

           IF DTU-OK AND DTU-RESULT-NUMBER > 0
               MOVE DTU-RESULT-NUMBER TO WS-INT-DAYS
           ELSE
               MOVE ZERO TO WS-INT-DAYS
           END-IF.

      *----------------------------------------------------------------*
      * 2430 - LOAN YEAR AT THE GOOD-THROUGH DATE (ANNIVERSARIES OF    *
      *        THE ORIGINATION DATE). 4 = AFTER THE THIRD ANNIVERSARY  *
      *----------------------------------------------------------------*
       2430-LOAN-YEAR.
           INITIALIZE DATE-PARMS
           MOVE 'ADDM'          TO DTU-FUNCTION
           MOVE LOAN-ORIG-DATE  TO DTU-DATE-1
           MOVE 12              TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-ANNIV-1
           MOVE 24              TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-ANNIV-2
           MOVE 36              TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-ANNIV-3

           EVALUATE TRUE
               WHEN WS-GT-DATE < WS-ANNIV-1
                   MOVE 1 TO WS-LOAN-YEAR
               WHEN WS-GT-DATE < WS-ANNIV-2
                   MOVE 2 TO WS-LOAN-YEAR
               WHEN WS-GT-DATE < WS-ANNIV-3
                   MOVE 3 TO WS-LOAN-YEAR
               WHEN OTHER
                   MOVE 4 TO WS-LOAN-YEAR
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2440 - PREPAYMENT PENALTY                                      *
      *   LNTM  YEAR 1 3.00% / YEAR 2 2.00% / YEAR 3 1.00% OF PRINCIPAL*
      *   LPER, LLOC, LCRE - NONE                                      *
      *----------------------------------------------------------------*
       2440-PREPAYMENT-PENALTY.
           MOVE ZERO   TO WS-PREPAY-PENALTY WS-PPP-PCT-USED
           MOVE SPACES TO WS-PP-DESC

           EVALUATE TRUE
               WHEN LOAN-IS-TERM AND WS-LOAN-YEAR < 4
                   MOVE WS-PPP-PCT(WS-LOAN-YEAR) TO WS-PPP-PCT-USED
                   COMPUTE WS-PREPAY-PENALTY ROUNDED =
                           WS-PRINCIPAL * WS-PPP-PCT-USED / 100
                   MOVE WS-PPP-PCT-USED TO WS-PCT-ED
                   STRING 'LOAN YEAR '  DELIMITED BY SIZE
                          WS-LOAN-YEAR  DELIMITED BY SIZE
                          ' - '         DELIMITED BY SIZE
                          WS-PCT-ED     DELIMITED BY SIZE
                          '% OF PRINCIPAL BALANCE'
                                        DELIMITED BY SIZE
                     INTO WS-PP-DESC
                   END-STRING
               WHEN LOAN-IS-TERM
                   MOVE 'PAST 3RD ANNIVERSARY - NO PENALTY'
                       TO WS-PP-DESC
               WHEN LOAN-IS-PERSONAL
                   MOVE 'NO PREPAYMENT PENALTY - CONSUMER'
                       TO WS-PP-DESC
               WHEN LOAN-IS-REVOLVER
                   MOVE 'NO PREPAYMENT PENALTY - REVOLVING LINE'
                       TO WS-PP-DESC
               WHEN LOAN-IS-CRE
                   MOVE 'NO PREPAYMENT PENALTY - CRE'
                       TO WS-PP-DESC
               WHEN OTHER
                   MOVE 'NO PREPAYMENT PENALTY' TO WS-PP-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2450 - NON-ACCRUAL LOAN (STATUS N)                             *
      *   PAYOFF = PRINCIPAL + LATE FEES + INTEREST CONTRACTUALLY OWED *
      *   (NON-ACCRUAL MEMO). NO PER DIEM - INTEREST IS NOT ACCRUING.  *
      *----------------------------------------------------------------*
       2450-NONACCRUAL-PAYOFF.
           MOVE LOAN-PRINCIPAL-BAL    TO WS-PRINCIPAL
           MOVE LOAN-LATE-FEES-OUTST  TO WS-LATE-FEES
           MOVE LOAN-NONACCR-INT-MEMO TO WS-NONACCR-INT
           MOVE ZERO TO WS-ACCRUED-INT WS-PER-DIEM WS-INT-DAYS
                        WS-INT-TO-GT WS-PREPAY-PENALTY WS-SUSPENSE
           PERFORM 2430-LOAN-YEAR
           MOVE 'NON-ACCRUAL - NO PER DIEM, INTEREST PER MEMO'
               TO WS-PP-DESC

           COMPUTE WS-PAYOFF-TOTAL = WS-PRINCIPAL
                                   + WS-LATE-FEES
                                   + WS-NONACCR-INT

           MOVE 'NON-ACCRUAL LOAN - CONFIRM PAYOFF WITH SPECIAL ASSETS'
               TO WS-MESSAGE.

      *----------------------------------------------------------------*
      * 2500 - FORMAT THE QUOTE                                        *
      *----------------------------------------------------------------*
       2500-FORMAT-QUOTE.
           MOVE LOAN-PRODUCT-CODE TO PRODCDO
           EVALUATE TRUE
               WHEN LOAN-IS-PERSONAL
                   MOVE 'PERSONAL INSTALLMENT' TO PRODDSCO
               WHEN LOAN-IS-TERM
                   MOVE 'COMMERCIAL TERM LOAN' TO PRODDSCO
               WHEN LOAN-IS-REVOLVER
                   MOVE 'REVOLVING LINE'       TO PRODDSCO
               WHEN LOAN-IS-CRE
                   MOVE 'COMMERCIAL REAL EST'  TO PRODDSCO
               WHEN OTHER
                   MOVE 'UNKNOWN PRODUCT'      TO PRODDSCO
           END-EVALUATE
           EVALUATE TRUE
               WHEN LOAN-ACTIVE
                   MOVE 'ACTIVE'              TO STATDSCO
               WHEN LOAN-DELINQUENT
                   MOVE 'DELINQUENT'          TO STATDSCO
               WHEN LOAN-NON-ACCRUAL
                   MOVE 'NON-ACCRUAL'         TO STATDSCO
               WHEN LOAN-FORBEARANCE
                   MOVE 'FORBEARANCE (CARES)' TO STATDSCO
           END-EVALUATE

           MOVE LOAN-BORROWER-CUST-ID TO BORRIDO
           MOVE WS-BORROWER-NAME      TO BORRNMO
           MOVE LOAN-INT-RATE         TO WS-RATE-ED
           MOVE WS-RATE-DISP          TO RATEO
           MOVE LOAN-ORIG-DATE        TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT       TO ORIGDTO
           IF WS-LOAN-YEAR > 3
               MOVE '4+'              TO LOANYRO
           ELSE
               MOVE WS-LOAN-YEAR      TO LOANYRO
           END-IF
           MOVE WS-BUS-DATE           TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT       TO BUSDTO
           MOVE LOAN-LAST-ACCRUAL-DATE TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT       TO LACCDTO
           MOVE WS-GT-DATE            TO WS-FMT-DATE-IN GTDATEO
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT       TO GTDISPO

           MOVE WS-PRINCIPAL          TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO PRINO
           MOVE WS-ACCRUED-INT        TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO ACCINTO
           MOVE WS-PER-DIEM           TO WS-PERDIEM-ED
           MOVE WS-PERDIEM-ED         TO PERDIEMO
           MOVE WS-INT-DAYS           TO WS-DAYS-ED
           MOVE WS-DAYS-ED            TO DAYSO
           MOVE WS-INT-TO-GT          TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO INTGTO
           MOVE WS-NONACCR-INT        TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO NAINTO
           MOVE WS-LATE-FEES          TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO LATEFEEO
           MOVE WS-PREPAY-PENALTY     TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO PPPENO
           MOVE WS-PP-DESC            TO PPDESCO
           MOVE WS-SUSPENSE           TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO SUSPO
           MOVE WS-PAYOFF-TOTAL       TO WS-AMT18-ED
           MOVE WS-AMT18-ED           TO TOTALO.

      *================================================================*
      * 8000 - COMMON ROUTINES                                         *
      *================================================================*
       8100-FORMAT-DATE.
           IF WS-FMT-DATE-IN = ZERO
               MOVE SPACES TO WS-FMT-DATE-OUT
           ELSE
               STRING WS-FMT-MM   DELIMITED BY SIZE
                      '/'         DELIMITED BY SIZE
                      WS-FMT-DD   DELIMITED BY SIZE
                      '/'         DELIMITED BY SIZE
                      WS-FMT-CCYY DELIMITED BY SIZE
                 INTO WS-FMT-DATE-OUT
               END-STRING
           END-IF.

       8800-SET-HEADER.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DISP-DATE)
                     DATESEP('/')
                     RESP(WS-RESP)
           END-EXEC
           MOVE WS-DISP-DATE TO CURDATEO.

       8900-SEND-MAP.
           PERFORM 8800-SET-HEADER
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('LNPAYQM')
                     MAPSET('LNPAYQS')
                     FROM(LNPAYQMO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND ABCODE('LN41') END-EXEC
           END-IF.

      *================================================================*
      * 9000 - TERMINATION AND ERROR HANDLING                          *
      *================================================================*
       9000-RETURN-TRANSID.
           MOVE WS-PROGRAM-ID TO CA-PROGRAM
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       9100-RETURN-TO-MENU.
           EXEC CICS XCTL PROGRAM('CBMENU')
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           MOVE LOW-VALUES TO LNPAYQMO
           MOVE 'MAIN MENU (CBMENU) NOT AVAILABLE - CONTACT HELP DESK'
               TO WS-MESSAGE
           PERFORM 8900-SEND-MAP.

       9900-CICS-ERROR.
           SET WS-ERROR TO TRUE
           MOVE WS-RESP  TO WS-RESP-D
           MOVE WS-RESP2 TO WS-RESP2-D
           MOVE SPACES TO WS-MESSAGE
           STRING 'CICS ERROR RESP=' DELIMITED BY SIZE
                  WS-RESP-D          DELIMITED BY SIZE
                  ' RESP2='          DELIMITED BY SIZE
                  WS-RESP2-D         DELIMITED BY SIZE
                  ' - CONTACT HELP DESK' DELIMITED BY SIZE
             INTO WS-MESSAGE
           END-STRING.
