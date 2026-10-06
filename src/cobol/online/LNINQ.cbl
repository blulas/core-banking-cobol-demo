      *================================================================*
      * PROGRAM  : LNINQ                                               *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANSACTION : CB40                                             *
      * MAPSET   : LNINQUS   MAP LNINQUM                               *
      * PURPOSE  : LOAN INQUIRY. DISPLAYS ONE LOAN FROM THE LOAN       *
      *            MASTER - BALANCES, RATE TERMS, PAYMENT TERMS,       *
      *            DELINQUENCY POSITION, COMMERCIAL RISK DATA AND THE  *
      *            FIVE MOST RECENT PAYMENTS FROM DB2.                 *
      *            THE LOAN IS SELECTED BY LOAN NUMBER, OR BY BORROWER *
      *            CUSTOMER ID (FIRST OPEN LOAN FOR THE BORROWER VIA   *
      *            THE LOANMST ALTERNATE INDEX PATH LOANAIX).          *
      * FILES    : LOANMST  (KSDS)        READ                         *
      *            LOANAIX  (AIX PATH)    STARTBR / READNEXT / ENDBR   *
      *            CUSTMST  (KSDS)        READ                         *
      * TABLES   : CBS.LOAN_PAYMENT_HIST  CURSOR LNPAY-CSR (READ ONLY) *
      * CALLS    : DELQBKT  (DLQPARM)  - BUCKET DESCRIPTION            *
      * XCTL     : CBMENU   (PF3)                                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2008-10-14 DPS  ORIGINAL - REPLACES CLS SCREEN L010            *
      * 2009-08-03 DPS  TIMES 30/60/90 COUNTERS ON SCREEN              *
      * 2011-02-22 DPS  BROWSE BY BORROWER CUST ID VIA LOANAIX         *
      *                 - INC10422                                     *
      * 2012-04-30 DPS  NON-ACCRUAL STATUS 'N' DECODE - CHG1187        *
      * 2014-06-16 BHS  BUCKET DESCRIPTION FROM DELQBKT INSTEAD OF     *
      *                 LOCAL TABLE - CHG1420                          *
      * 2016-04-25 AKW  LPER FIELDS - AUTOPAY ACCT, BORROWER STATE     *
      * 2018-11-05 AKW  RESP/RESP2 ON ALL CICS COMMANDS - AUDIT A18-07 *
      * 2020-04-06 AKW  FORBEARANCE STATUS 'F' DISPLAY (CARES ACT)     *
      * 2022-01-10 AKW  SOFR INDEX DISPLAY - REG-2022-L01              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNINQ.
       AUTHOR. D P SANDOVAL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNINQ'.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB40'.

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
           05  WS-BROWSE-FLAG              PIC X(01) VALUE 'N'.
               88  WS-BROWSE-DONE                    VALUE 'Y'.
               88  WS-BROWSE-MORE                    VALUE 'N'.
           05  WS-SQL-EOF-FLAG             PIC X(01) VALUE 'N'.
               88  WS-SQL-EOF                        VALUE 'Y'.

       01  WS-MESSAGE                      PIC X(79) VALUE SPACES.

       01  WS-KEYS.
           05  WS-LOAN-KEY                 PIC X(12).
           05  WS-AIX-KEY                  PIC X(10).
           05  WS-CUST-KEY                 PIC X(10).

      *    BROWSE RESULTS (BORROWER CUST ID PATH)
       01  WS-BROWSE-WORK.
           05  WS-BR-COUNT                 PIC 9(03) COMP-3 VALUE 0.
      *        2011-02 SAFETY LIMIT - A BORROWER WITH MORE THAN 50
      *        LOANS IS A COMMERCIAL RELATIONSHIP, USE CLS L020
           05  WS-BR-MAX                   PIC 9(03) COMP-3 VALUE 50.
           05  WS-BR-FIRST-LOAN            PIC X(12).
           05  WS-BR-FIRST-OPEN            PIC X(12).
           05  WS-BR-COUNT-ED              PIC ZZ9.

      *    COMMAREA - SAVED BETWEEN PSEUDO-CONVERSATIONAL TASKS
       01  WS-COMMAREA.
           05  CA-PROGRAM                  PIC X(08).
           05  CA-LOAN-NUMBER              PIC X(12).
           05  CA-CUST-ID                  PIC X(10).
           05  FILLER                      PIC X(10).

      *    EDITED DISPLAY FIELDS
       01  WS-EDIT-FIELDS.
           05  WS-AMT-WORK                 PIC S9(13)V99 COMP-3.
           05  WS-AMT18-ED                 PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AMT14-ED                 PIC ZZ,ZZZ,ZZ9.99-.
           05  WS-AMT12-ED                 PIC ZZZ,ZZ9.99-.
           05  WS-RATE-DISP.
               10  WS-RATE-ED              PIC ZZ9.9999.
               10  FILLER                  PIC X(01) VALUE '%'.
           05  WS-DSCR-ED                  PIC Z9.99.
           05  WS-LTV-ED                   PIC ZZ9.99.
           05  WS-DPD-ED                   PIC ZZZ9.
           05  WS-CNT-ED                   PIC Z9.
           05  WS-SQLCODE-ED               PIC -(4)9.

      *    DATE FORMAT WORK  CCYYMMDD -> MM/DD/CCYY
       01  WS-DATE-WORK.
           05  WS-FMT-DATE-IN              PIC 9(08).
           05  WS-FMT-DATE-R REDEFINES WS-FMT-DATE-IN.
               10  WS-FMT-CCYY             PIC 9(04).
               10  WS-FMT-MM               PIC 9(02).
               10  WS-FMT-DD               PIC 9(02).
           05  WS-FMT-DATE-OUT             PIC X(10).
      *    DB2 DATE ISO FORMAT CCYY-MM-DD
           05  WS-ISO-DATE.
               10  WS-ISO-CCYY             PIC X(04).
               10  FILLER                  PIC X(01).
               10  WS-ISO-MM               PIC X(02).
               10  FILLER                  PIC X(01).
               10  WS-ISO-DD               PIC X(02).

       01  WS-BORROWER-NAME                PIC X(30).

      *    PAYMENT HISTORY LINES (5 MOST RECENT)
       01  WS-HIST-IX                      PIC 9(02) COMP-3.
       01  WS-HIST-TABLE.
           05  WS-HIST-ENTRY               OCCURS 5 TIMES
                                           PIC X(76).
       01  WS-HIST-LINE.
           05  HL-DATE                     PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-AMOUNT                   PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-FEES                     PIC ZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-INTEREST                 PIC Z,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-PRINCIPAL                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-PRIN-AFTER               PIC ZZZ,ZZZ,ZZ9.99.

      *    COPYBOOKS
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY LNINQUS.
       COPY LOANREC.
       COPY CUSTREC.
       COPY DLQPARM.

      *    DB2
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLLNPY END-EXEC.

           EXEC SQL DECLARE LNPAY-CSR CURSOR FOR
                SELECT PAYMENT_DATE
                     , PAYMENT_AMT
                     , APPLIED_FEES
                     , APPLIED_INT
                     , APPLIED_PRIN
                     , PRIN_BAL_AFTER
                     , TRAN_ID
                  FROM CBS.LOAN_PAYMENT_HIST
                 WHERE LOAN_NUMBER = :LPY-LOAN-NUMBER
                 ORDER BY PAYMENT_DATE DESC, TRAN_ID DESC
                 FETCH FIRST 5 ROWS ONLY
                 FOR FETCH ONLY
           END-EXEC.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(40).

       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
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
                       PERFORM 2000-PROCESS-INQUIRY
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
      * 1000 - FIRST ENTRY OR CLEAR - SEND EMPTY MAP                   *
      *================================================================*
       1000-FIRST-TIME.
           MOVE LOW-VALUES TO LNINQUMO
           INITIALIZE WS-COMMAREA
           MOVE WS-PROGRAM-ID TO CA-PROGRAM
           MOVE 'ENTER LOAN NUMBER OR BORROWER CUSTOMER ID'
               TO WS-MESSAGE
           MOVE -1 TO LOANNOL
           PERFORM 8900-SEND-MAP.

      *================================================================*
      * 2000 - ENTER PRESSED - EDIT, READ LOAN, FORMAT                 *
      *================================================================*
       2000-PROCESS-INQUIRY.
           SET WS-NO-ERROR TO TRUE
           PERFORM 2050-RECEIVE-MAP

           IF WS-NO-ERROR
               PERFORM 2100-EDIT-INPUT
           END-IF

           IF WS-NO-ERROR
               IF LOANNOI NOT = SPACES
                   MOVE LOANNOI TO WS-LOAN-KEY
                   PERFORM 2200-READ-LOAN
               ELSE
                   PERFORM 2300-BROWSE-BY-CUSTOMER
               END-IF
           END-IF

           IF WS-NO-ERROR
               MOVE LOAN-NUMBER TO CA-LOAN-NUMBER LOANNOO
               PERFORM 2400-READ-BORROWER
               PERFORM 2500-FORMAT-LOAN
               PERFORM 2600-DELINQUENCY-BUCKET
               PERFORM 2700-PAYMENT-HISTORY
           END-IF

           PERFORM 8900-SEND-MAP.

       2050-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('LNINQUM')
                     MAPSET('LNINQUS')
                     INTO(LNINQUMI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE LOW-VALUES TO LNINQUMI
                   SET WS-ERROR TO TRUE
                   MOVE 'ENTER LOAN NUMBER OR BORROWER CUSTOMER ID'
                       TO WS-MESSAGE
                   MOVE -1 TO LOANNOL
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE

           INSPECT LOANNOI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT CUSTIDI REPLACING ALL LOW-VALUE BY SPACE.

      *----------------------------------------------------------------*
      * 2100 - SCREEN EDITS                                            *
      *   LOAN NUMBER TAKES PRECEDENCE WHEN BOTH KEYS ARE ENTERED.     *
      *   LOAN NUMBERS ARE 12 DIGITS BEGINNING WITH 3.                 *
      *----------------------------------------------------------------*
       2100-EDIT-INPUT.
           EVALUATE TRUE
               WHEN LOANNOI = SPACES AND CUSTIDI = SPACES
                   SET WS-ERROR TO TRUE
                   MOVE 'ENTER LOAN NUMBER OR BORROWER CUSTOMER ID'
                       TO WS-MESSAGE
                   MOVE -1 TO LOANNOL
               WHEN LOANNOI NOT = SPACES
                AND LOANNOI IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'LOAN NUMBER MUST BE 12 DIGITS' TO WS-MESSAGE
                   MOVE DFHUNIMD TO LOANNOA
                   MOVE -1 TO LOANNOL
               WHEN LOANNOI NOT = SPACES
                AND LOANNOI(1:1) NOT = '3'
                   SET WS-ERROR TO TRUE
                   MOVE 'NOT A LOAN NUMBER - LOANS BEGIN WITH 3'
                       TO WS-MESSAGE
                   MOVE DFHUNIMD TO LOANNOA
                   MOVE -1 TO LOANNOL
               WHEN LOANNOI = SPACES
                AND CUSTIDI(1:1) NOT = 'C'
                   SET WS-ERROR TO TRUE
                   MOVE 'CUSTOMER ID FORMAT IS CNNNNNNNNN'
                       TO WS-MESSAGE
                   MOVE DFHUNIMD TO CUSTIDA
                   MOVE -1 TO CUSTIDL
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2200 - READ LOAN MASTER BY LOAN NUMBER                         *
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
      * 2300 - BROWSE LOANAIX BY BORROWER CUST ID. SELECTS THE FIRST   *
      *        OPEN LOAN (STATUS A/D/N/F); IF NONE ARE OPEN, THE FIRST *
      *        LOAN FOUND (PAID OFF / CHARGED OFF) IS SHOWN.           *
      *----------------------------------------------------------------*
       2300-BROWSE-BY-CUSTOMER.
           MOVE CUSTIDI TO WS-AIX-KEY
           MOVE ZERO    TO WS-BR-COUNT
           MOVE SPACES  TO WS-BR-FIRST-LOAN WS-BR-FIRST-OPEN
           SET WS-BROWSE-MORE TO TRUE

           EXEC CICS STARTBR FILE('LOANAIX')
                     RIDFLD(WS-AIX-KEY)
                     KEYLENGTH(10)
                     GTEQ
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2310-READNEXT-LOAN
                       UNTIL WS-BROWSE-DONE
                   EXEC CICS ENDBR FILE('LOANAIX')
                             RESP(WS-RESP)
                   END-EXEC
               WHEN DFHRESP(NOTFND)
                   CONTINUE
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE

           IF WS-NO-ERROR
               EVALUATE TRUE
                   WHEN WS-BR-COUNT = 0
                       SET WS-ERROR TO TRUE
                       MOVE 'NO LOANS FOUND FOR THIS CUSTOMER'
                           TO WS-MESSAGE
                       MOVE -1 TO CUSTIDL
                   WHEN WS-BR-FIRST-OPEN NOT = SPACES
                       MOVE WS-BR-FIRST-OPEN TO WS-LOAN-KEY
                   WHEN OTHER
                       MOVE WS-BR-FIRST-LOAN TO WS-LOAN-KEY
               END-EVALUATE
           END-IF

           IF WS-NO-ERROR
               PERFORM 2200-READ-LOAN
           END-IF

           IF WS-NO-ERROR AND WS-BR-COUNT > 1
               MOVE WS-BR-COUNT TO WS-BR-COUNT-ED
               MOVE SPACES TO WS-MESSAGE
               STRING 'CUSTOMER HAS ' DELIMITED BY SIZE
                      WS-BR-COUNT-ED  DELIMITED BY SIZE
                      ' LOANS - SHOWING FIRST OPEN LOAN'
                                      DELIMITED BY SIZE
                 INTO WS-MESSAGE
               END-STRING
           END-IF.

       2310-READNEXT-LOAN.
           EXEC CICS READNEXT FILE('LOANAIX')
                     INTO(LOAN-RECORD)
                     RIDFLD(WS-AIX-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(DUPKEY)
                   IF LOAN-BORROWER-CUST-ID NOT = CUSTIDI
                       SET WS-BROWSE-DONE TO TRUE
                   ELSE
                       ADD 1 TO WS-BR-COUNT
                       IF WS-BR-FIRST-LOAN = SPACES
                           MOVE LOAN-NUMBER TO WS-BR-FIRST-LOAN
                       END-IF
                       IF LOAN-IS-OPEN
                          AND WS-BR-FIRST-OPEN = SPACES
                           MOVE LOAN-NUMBER TO WS-BR-FIRST-OPEN
                       END-IF
                       IF WS-BR-COUNT >= WS-BR-MAX
                           SET WS-BROWSE-DONE TO TRUE
                       END-IF
                   END-IF
               WHEN DFHRESP(ENDFILE)
                   SET WS-BROWSE-DONE TO TRUE
               WHEN OTHER
                   SET WS-BROWSE-DONE TO TRUE
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2400 - BORROWER NAME FROM THE CIF                              *
      *----------------------------------------------------------------*
       2400-READ-BORROWER.
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
               WHEN DFHRESP(NOTFND)
                   MOVE '*** CIF RECORD NOT FOUND ***'
                       TO WS-BORROWER-NAME
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE

           MOVE LOAN-BORROWER-CUST-ID    TO BORRIDO
           MOVE WS-BORROWER-NAME         TO BORRNMO
           MOVE LOAN-CO-BORROWER-CUST-ID TO COBORRO.

      *----------------------------------------------------------------*
      * 2500 - FORMAT LOAN DETAIL                                      *
      *----------------------------------------------------------------*
       2500-FORMAT-LOAN.
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

      *    STATUS DECODE
           MOVE LOAN-STATUS TO STATCDO
           EVALUATE TRUE
               WHEN LOAN-ACTIVE
                   MOVE 'ACTIVE'               TO STATDSCO
               WHEN LOAN-PAID-OFF
                   MOVE 'PAID OFF'             TO STATDSCO
                   MOVE LOAN-PAYOFF-DATE TO WS-FMT-DATE-IN
                   PERFORM 8100-FORMAT-DATE
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'LOAN PAID OFF ' DELIMITED BY SIZE
                          WS-FMT-DATE-OUT  DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
               WHEN LOAN-DELINQUENT
                   MOVE 'DELINQUENT'           TO STATDSCO
               WHEN LOAN-CHARGED-OFF
                   MOVE 'CHARGED OFF'          TO STATDSCO
                   MOVE LOAN-CHARGEOFF-DATE TO WS-FMT-DATE-IN
                   PERFORM 8100-FORMAT-DATE
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'CHARGED OFF ' DELIMITED BY SIZE
                          WS-FMT-DATE-OUT DELIMITED BY SIZE
                          ' - REFER TO RECOVERY' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
               WHEN LOAN-NON-ACCRUAL
                   MOVE 'NON-ACCRUAL'          TO STATDSCO
               WHEN LOAN-FORBEARANCE
                   MOVE 'FORBEARANCE (CARES)'  TO STATDSCO
               WHEN OTHER
                   MOVE 'UNKNOWN STATUS'       TO STATDSCO
           END-EVALUATE
           IF LOAN-DELINQUENT OR LOAN-CHARGED-OFF OR LOAN-NON-ACCRUAL
               MOVE DFHBMASB TO STATCDA STATDSCA
           END-IF

      *    BALANCES
           MOVE LOAN-PRINCIPAL-BAL TO WS-AMT18-ED
           MOVE WS-AMT18-ED        TO PRINBALO
      *    ACCRUED INTEREST CARRIED AT 6 DECIMALS - SHOW TO THE CENT
           COMPUTE WS-AMT-WORK ROUNDED = LOAN-ACCRUED-INT
           MOVE WS-AMT-WORK        TO WS-AMT18-ED
           MOVE WS-AMT18-ED        TO ACCRINTO
           MOVE LOAN-ORIG-AMOUNT   TO WS-AMT18-ED
           MOVE WS-AMT18-ED        TO ORIGAMTO

      *    RATE TERMS
           MOVE LOAN-INT-RATE      TO WS-RATE-ED
           MOVE WS-RATE-DISP       TO RATEO
           EVALUATE TRUE
               WHEN LOAN-FIXED-RATE
                   MOVE 'FIXED'    TO RTTYPEO
                   MOVE SPACES     TO RTINDXO MARGINO
               WHEN LOAN-VARIABLE-RATE
                   MOVE 'VARIABLE' TO RTTYPEO
                   MOVE LOAN-RATE-INDEX  TO RTINDXO
                   MOVE LOAN-MARGIN-RATE TO WS-RATE-ED
                   MOVE WS-RATE-DISP     TO MARGINO
               WHEN OTHER
                   MOVE LOAN-RATE-TYPE   TO RTTYPEO
           END-EVALUATE

      *    COMMITMENT / AVAILABLE - REVOLVING LINES ONLY
           IF LOAN-IS-REVOLVER
               MOVE LOAN-COMMITMENT-AMT TO WS-AMT18-ED
               MOVE WS-AMT18-ED         TO COMMITO
               COMPUTE WS-AMT-WORK = LOAN-COMMITMENT-AMT
                                   - LOAN-PRINCIPAL-BAL
               IF WS-AMT-WORK < 0
                   MOVE WS-AMT-WORK TO WS-AMT14-ED
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'LINE IS OVER COMMITMENT BY '
                                         DELIMITED BY SIZE
                          WS-AMT14-ED    DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
                   MOVE ZERO TO WS-AMT-WORK
               END-IF
               MOVE WS-AMT-WORK         TO WS-AMT18-ED
               MOVE WS-AMT18-ED         TO AVAILO
           ELSE
               MOVE 'N/A'               TO COMMITO AVAILO
           END-IF

      *    DATES
           MOVE LOAN-ORIG-DATE     TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT    TO ORIGDTO
           MOVE LOAN-MATURITY-DATE TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT    TO MATDTO
           MOVE LOAN-NEXT-DUE-DATE TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT    TO NXTDUEO
           MOVE LOAN-LAST-PMT-DATE TO WS-FMT-DATE-IN
           PERFORM 8100-FORMAT-DATE
           MOVE WS-FMT-DATE-OUT    TO LSTPDTO

      *    PAYMENT TERMS
           MOVE LOAN-PAYMENT-AMT   TO WS-AMT14-ED
           MOVE WS-AMT14-ED        TO PMTAMTO
           EVALUATE TRUE
               WHEN LOAN-PAY-MONTHLY
                   MOVE 'MONTHLY'   TO PMTFRQO
               WHEN LOAN-PAY-QUARTERLY
                   MOVE 'QUARTERLY' TO PMTFRQO
               WHEN OTHER
                   MOVE LOAN-PAYMENT-FREQ TO PMTFRQO
           END-EVALUATE
           MOVE LOAN-LAST-PMT-AMT  TO WS-AMT14-ED
           MOVE WS-AMT14-ED        TO LSTPAMTO

      *    AUTOPAY
           IF LOAN-ON-AUTOPAY
               MOVE 'Y'               TO AUTOPFO
               MOVE LOAN-AUTOPAY-ACCT TO AUTOACO
           ELSE
               MOVE 'N'               TO AUTOPFO
               MOVE SPACES            TO AUTOACO
           END-IF

      *    PAST DUE / DELINQUENCY
           MOVE LOAN-PAST-DUE-PRIN   TO WS-AMT14-ED
           MOVE WS-AMT14-ED          TO PDPRINO
           MOVE LOAN-PAST-DUE-INT    TO WS-AMT12-ED
           MOVE WS-AMT12-ED          TO PDINTO
           MOVE LOAN-PAST-DUE-FEES   TO WS-AMT12-ED
           MOVE WS-AMT12-ED          TO PDFEESO
           MOVE LOAN-LATE-FEES-OUTST TO WS-AMT12-ED
           MOVE WS-AMT12-ED          TO LATEFEEO
           MOVE LOAN-DAYS-PAST-DUE   TO WS-DPD-ED
           MOVE WS-DPD-ED            TO DPDO
           IF LOAN-DAYS-PAST-DUE > 29
               MOVE DFHBMASB TO DPDA
           END-IF
           MOVE LOAN-TIMES-30-DPD    TO WS-CNT-ED
           MOVE WS-CNT-ED            TO T30O
           MOVE LOAN-TIMES-60-DPD    TO WS-CNT-ED
           MOVE WS-CNT-ED            TO T60O
           MOVE LOAN-TIMES-90-DPD    TO WS-CNT-ED
           MOVE WS-CNT-ED            TO T90O

      *    COMMERCIAL RISK DATA - LNTM / LLOC / LCRE ONLY
           IF LOAN-IS-COMMERCIAL
               MOVE LOAN-RISK-GRADE      TO RISKGRO
               MOVE LOAN-DSCR            TO WS-DSCR-ED
               MOVE WS-DSCR-ED           TO DSCRO
               MOVE LOAN-LTV-PCT         TO WS-LTV-ED
               MOVE WS-LTV-ED            TO LTVO
               MOVE LOAN-COLLATERAL-TYPE TO COLLTYO
           ELSE
               MOVE SPACES TO RISKGRO DSCRO LTVO COLLTYO
           END-IF.

      *----------------------------------------------------------------*
      * 2600 - BUCKET DESCRIPTION FROM THE SHARED DELINQUENCY RULES    *
      *        (SAME MODULE AS BATCH LNDELQ). INQUIRY ONLY - THE       *
      *        STATUS / BUREAU OUTPUTS ARE IGNORED HERE.               *
      *----------------------------------------------------------------*
       2600-DELINQUENCY-BUCKET.
           INITIALIZE DELQ-PARMS
           MOVE 'AGE '                TO DLQ-FUNCTION
           MOVE LOAN-PRODUCT-CODE     TO DLQ-PRODUCT-CODE
           MOVE LOAN-CATEGORY         TO DLQ-CATEGORY
           MOVE LOAN-STATUS           TO DLQ-CURRENT-STATUS
           MOVE LOAN-DAYS-PAST-DUE    TO DLQ-DAYS-PAST-DUE
                                         DLQ-PRIOR-DPD
           MOVE LOAN-PAYMENT-AMT      TO DLQ-INSTALLMENT-AMT
           MOVE LOAN-BORROWER-STATE   TO DLQ-STATE

           CALL 'DELQBKT' USING DELQ-PARMS

           IF DLQ-OK
               MOVE DLQ-BUCKET-DESC TO BUCKETO
           ELSE
      *        2014-06 BHS FALL BACK TO THE STORED BUCKET CODE
               MOVE SPACES TO BUCKETO
               STRING 'BUCKET ' DELIMITED BY SIZE
                      LOAN-DELQ-BUCKET DELIMITED BY SIZE
                 INTO BUCKETO
               END-STRING
           END-IF
           IF LOAN-DAYS-PAST-DUE > 29
               MOVE DFHBMASB TO BUCKETA
           END-IF.

      *----------------------------------------------------------------*
      * 2700 - LAST 5 PAYMENTS FROM CBS.LOAN_PAYMENT_HIST              *
      *        TABLE IS LOADED NIGHTLY - TODAY'S PAYMENTS SHOW TOMORROW*
      *----------------------------------------------------------------*
       2700-PAYMENT-HISTORY.
           MOVE SPACES      TO WS-HIST-TABLE
           MOVE 'N'         TO WS-SQL-EOF-FLAG
           MOVE LOAN-NUMBER TO LPY-LOAN-NUMBER

           EXEC SQL OPEN LNPAY-CSR END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 2790-SQL-ERROR
           ELSE
               PERFORM 2710-FETCH-PAYMENT
                   VARYING WS-HIST-IX FROM 1 BY 1
                     UNTIL WS-HIST-IX > 5 OR WS-SQL-EOF
               EXEC SQL CLOSE LNPAY-CSR END-EXEC
           END-IF

           MOVE WS-HIST-ENTRY(1) TO HIST1O
           MOVE WS-HIST-ENTRY(2) TO HIST2O
           MOVE WS-HIST-ENTRY(3) TO HIST3O
           MOVE WS-HIST-ENTRY(4) TO HIST4O
           MOVE WS-HIST-ENTRY(5) TO HIST5O.

       2710-FETCH-PAYMENT.
           EXEC SQL FETCH LNPAY-CSR
                     INTO :LPY-PAYMENT-DATE
                        , :LPY-PAYMENT-AMT
                        , :LPY-APPLIED-FEES
                        , :LPY-APPLIED-INT
                        , :LPY-APPLIED-PRIN
                        , :LPY-PRIN-BAL-AFTER
                        , :LPY-TRAN-ID
           END-EXEC

           EVALUATE TRUE
               WHEN SQLCODE = 0
                   MOVE LPY-PAYMENT-DATE   TO WS-ISO-DATE
                   STRING WS-ISO-MM   DELIMITED BY SIZE
                          '/'         DELIMITED BY SIZE
                          WS-ISO-DD   DELIMITED BY SIZE
                          '/'         DELIMITED BY SIZE
                          WS-ISO-CCYY DELIMITED BY SIZE
                     INTO HL-DATE
                   END-STRING
                   MOVE LPY-PAYMENT-AMT    TO HL-AMOUNT
                   MOVE LPY-APPLIED-FEES   TO HL-FEES
                   MOVE LPY-APPLIED-INT    TO HL-INTEREST
                   MOVE LPY-APPLIED-PRIN   TO HL-PRINCIPAL
                   MOVE LPY-PRIN-BAL-AFTER TO HL-PRIN-AFTER
                   MOVE WS-HIST-LINE TO WS-HIST-ENTRY(WS-HIST-IX)
               WHEN SQLCODE = 100
                   SET WS-SQL-EOF TO TRUE
                   IF WS-HIST-IX = 1
                       MOVE '   NO PAYMENTS ON FILE'
                           TO WS-HIST-ENTRY(1)
                   END-IF
               WHEN OTHER
                   SET WS-SQL-EOF TO TRUE
                   PERFORM 2790-SQL-ERROR
           END-EVALUATE.

      *    DB2 ERRORS ARE NOT FATAL - THE LOAN DETAIL IS STILL SHOWN
       2790-SQL-ERROR.
           MOVE SQLCODE TO WS-SQLCODE-ED
           IF WS-MESSAGE = SPACES
               STRING 'PAYMENT HISTORY UNAVAILABLE - SQLCODE '
                                      DELIMITED BY SIZE
                      WS-SQLCODE-ED   DELIMITED BY SIZE
                 INTO WS-MESSAGE
               END-STRING
           END-IF
           MOVE '   *** PAYMENT HISTORY UNAVAILABLE ***'
               TO WS-HIST-ENTRY(1).

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
           EXEC CICS SEND MAP('LNINQUM')
                     MAPSET('LNINQUS')
                     FROM(LNINQUMO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND ABCODE('LN40') END-EXEC
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
      *    ONLY REACHED IF THE XCTL FAILED
           MOVE LOW-VALUES TO LNINQUMO
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
