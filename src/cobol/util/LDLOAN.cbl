      *================================================================*
      * PROGRAM  : LDLOAN                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE LOAN MASTER (LOANMST) FROM THE PIPE-      *
      *            DELIMITED CLS CONVERSION EXTRACT (LOANSEED), THEN   *
      *            LOADS THE BILLED-AND-UNPAID INSTALLMENTS (LDUESEED) *
      *            INTO EACH LOAN'S DUE TABLE AND DERIVES THE PAST-DUE *
      *            BUCKETS. ACCRUAL BASIS AND CATEGORY ARE TAKEN FROM  *
      *            THE PRODUCT MASTER.                                 *
      * INPUT    : LOANSEED  SEE COLUMN HEADER IN DATA/SEED/LOANS.PSV  *
      *            LDUESEED  LOAN|DUEDATE|INTEREST|PRINCIPAL|FEEFLAG   *
      *            PRODMST   (KSDS)                                    *
      * OUTPUT   : LOANMST   (KSDS + AIX ON BORROWER CUST ID)          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT (CLS->CBS)  *
      * 2015-11-19 DPS  CNV-0122 DUE TABLE FROM SEPARATE EXTRACT       *
      * 2016-04-25 AKW  LPER FIELDS - SCORE, AUTOPAY, STATE            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDLOAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "LOANSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT DUE-SEED-FILE ASSIGN TO "LDUESEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DUE-STATUS.
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT LOAN-FILE ASSIGN TO "LOANMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NUMBER
               ALTERNATE RECORD KEY IS LOAN-BORROWER-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-LOAN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(800).
       FD  DUE-SEED-FILE.
       01  DUE-SEED-LINE                   PIC X(200).
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  LOAN-FILE.
       COPY LOANREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-DUE-STATUS                   PIC X(02).
       01  WS-PROD-STATUS                  PIC X(02).
       01  WS-LOAN-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-DUE-COUNT                PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 48.
       01  WS-DUE-FIELDS.
           05  WS-DF                       PIC X(20) OCCURS 5.
       01  WS-I                            PIC 9(02).
       01  WS-N                            PIC 9(02).
       01  WS-DAYS                         PIC S9(07).
       COPY BANKCTL.
       COPY DATEPARM.
       COPY DLQPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           PERFORM 1000-LOAD-LOANS
           PERFORM 2000-LOAD-DUE-TABLE
           DISPLAY 'LDLOAN  READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT
                   ' DUE-ENTRIES=' WS-DUE-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           END-IF
           STOP RUN.

      *================================================================*
      * PASS 1 - LOAN MASTER                                           *
      *================================================================*
       1000-LOAD-LOANS.
           OPEN INPUT SEED-FILE
           OPEN INPUT PRODUCT-FILE
           OPEN OUTPUT LOAN-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-PROD-STATUS NOT = '00'
              OR WS-LOAN-STATUS NOT = '00'
               MOVE 'LDLOAN' TO ABND-PROGRAM
               MOVE '1000-LOAD-LOANS' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - LOANSEED/PRODMST/LOANMST'
                 TO ABND-MESSAGE
               MOVE WS-LOAN-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1100-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE PRODUCT-FILE LOAN-FILE.

       1100-PROCESS-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-READ-COUNT
           MOVE SPACES TO WS-FIELDS
           UNSTRING SEED-LINE DELIMITED BY '|'
               INTO WS-F(1)  WS-F(2)  WS-F(3)  WS-F(4)  WS-F(5)
                    WS-F(6)  WS-F(7)  WS-F(8)  WS-F(9)  WS-F(10)
                    WS-F(11) WS-F(12) WS-F(13) WS-F(14) WS-F(15)
                    WS-F(16) WS-F(17) WS-F(18) WS-F(19) WS-F(20)
                    WS-F(21) WS-F(22) WS-F(23) WS-F(24) WS-F(25)
                    WS-F(26) WS-F(27) WS-F(28) WS-F(29) WS-F(30)
                    WS-F(31) WS-F(32) WS-F(33) WS-F(34) WS-F(35)
                    WS-F(36) WS-F(37) WS-F(38) WS-F(39) WS-F(40)
                    WS-F(41) WS-F(42) WS-F(43) WS-F(44) WS-F(45)
                    WS-F(46) WS-F(47) WS-F(48)
           END-UNSTRING
           INITIALIZE LOAN-RECORD
           MOVE WS-F(1)  TO LOAN-NUMBER
           MOVE WS-F(2)  TO LOAN-PRODUCT-CODE
           MOVE WS-F(3)  TO LOAN-BORROWER-CUST-ID
           MOVE WS-F(4)  TO LOAN-CO-BORROWER-CUST-ID
           MOVE WS-F(5)  TO LOAN-STATUS
           COMPUTE LOAN-ORIG-AMOUNT    = FUNCTION NUMVAL(WS-F(6))
           COMPUTE LOAN-COMMITMENT-AMT = FUNCTION NUMVAL(WS-F(7))
           COMPUTE LOAN-PRINCIPAL-BAL  = FUNCTION NUMVAL(WS-F(8))
           COMPUTE LOAN-ACCRUED-INT    = FUNCTION NUMVAL(WS-F(9))
           COMPUTE LOAN-INT-RATE       = FUNCTION NUMVAL(WS-F(10))
           MOVE WS-F(11) TO LOAN-RATE-TYPE
           MOVE WS-F(12) TO LOAN-RATE-INDEX
           COMPUTE LOAN-MARGIN-RATE    = FUNCTION NUMVAL(WS-F(13))
           COMPUTE LOAN-FLOOR-RATE     = FUNCTION NUMVAL(WS-F(14))
           COMPUTE LOAN-CAP-RATE       = FUNCTION NUMVAL(WS-F(15))
           COMPUTE LOAN-ORIG-DATE      = FUNCTION NUMVAL(WS-F(16))
           COMPUTE LOAN-MATURITY-DATE  = FUNCTION NUMVAL(WS-F(17))
           COMPUTE LOAN-TERM-MONTHS    = FUNCTION NUMVAL(WS-F(18))
           COMPUTE LOAN-AMORT-MONTHS   = FUNCTION NUMVAL(WS-F(19))
           COMPUTE LOAN-IO-MONTHS      = FUNCTION NUMVAL(WS-F(20))
           COMPUTE LOAN-PAYMENT-AMT    = FUNCTION NUMVAL(WS-F(21))
           MOVE WS-F(22) TO LOAN-PAYMENT-FREQ
           COMPUTE LOAN-NEXT-DUE-DATE  = FUNCTION NUMVAL(WS-F(23))
           COMPUTE LOAN-INSTALLMENTS-BILLED =
                   FUNCTION NUMVAL(WS-F(24))
           COMPUTE LOAN-LAST-BILLED-DUE-DATE =
                   FUNCTION NUMVAL(WS-F(25))
           COMPUTE LOAN-LAST-PMT-DATE  = FUNCTION NUMVAL(WS-F(26))
           COMPUTE LOAN-LAST-PMT-AMT   = FUNCTION NUMVAL(WS-F(27))
           COMPUTE LOAN-SUSPENSE-BAL   = FUNCTION NUMVAL(WS-F(28))
           COMPUTE LOAN-LATE-FEES-OUTST = FUNCTION NUMVAL(WS-F(29))
           COMPUTE LOAN-LATE-FEE-COUNT = FUNCTION NUMVAL(WS-F(30))
           COMPUTE LOAN-DAYS-PAST-DUE  = FUNCTION NUMVAL(WS-F(31))
           COMPUTE LOAN-TIMES-30-DPD   = FUNCTION NUMVAL(WS-F(32))
           COMPUTE LOAN-TIMES-60-DPD   = FUNCTION NUMVAL(WS-F(33))
           COMPUTE LOAN-TIMES-90-DPD   = FUNCTION NUMVAL(WS-F(34))
           MOVE WS-F(35) TO LOAN-AUTOPAY-FLAG
           MOVE WS-F(36) TO LOAN-AUTOPAY-ACCT
           COMPUTE LOAN-CREDIT-SCORE   = FUNCTION NUMVAL(WS-F(37))
           COMPUTE LOAN-RISK-GRADE     = FUNCTION NUMVAL(WS-F(38))
           MOVE WS-F(39) TO LOAN-COLLATERAL-TYPE
           COMPUTE LOAN-COLLATERAL-VALUE = FUNCTION NUMVAL(WS-F(40))
           COMPUTE LOAN-LTV-PCT        = FUNCTION NUMVAL(WS-F(41))
           COMPUTE LOAN-DSCR           = FUNCTION NUMVAL(WS-F(42))
           MOVE WS-F(43) TO LOAN-COVENANT-FLAGS
           MOVE WS-F(44) TO LOAN-BORROWER-STATE
           MOVE WS-F(45) TO LOAN-COST-CENTER
           MOVE WS-F(46) TO LOAN-OFFICER-ID
           COMPUTE LOAN-LAST-ACCRUAL-DATE = FUNCTION NUMVAL(WS-F(47))
           COMPUTE LOAN-INT-PAID-YTD   = FUNCTION NUMVAL(WS-F(48))

           MOVE LOAN-ORIG-DATE      TO LOAN-STATUS-DATE
      *    CNV-0131 24 MONTH 30/60/90 WINDOW STARTS AT CONVERSION
           MOVE CTL-PREV-BUS-DATE   TO LOAN-DLQ-CTR-START-DATE
           MOVE LOAN-ORIG-DATE      TO LOAN-LAST-RATE-CHG-DATE
           MOVE CTL-PREV-BUS-DATE   TO LOAN-LAST-BILL-DATE
           MOVE ZERO                TO LOAN-DUE-COUNT
           MOVE '0'                 TO LOAN-DELQ-BUCKET
           MOVE 'N'                 TO LOAN-BUREAU-RPT-FLAG
           MOVE CTL-BUSINESS-DATE   TO LOAN-LAST-MAINT-DATE
           MOVE 'LDLOAN'            TO LOAN-LAST-MAINT-USER
           IF LOAN-AUTOPAY-FLAG NOT = 'Y'
               MOVE 'N' TO LOAN-AUTOPAY-FLAG
           END-IF

      *    ACCRUAL BASIS AND CATEGORY FROM THE PRODUCT MASTER
           MOVE LOAN-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
           IF WS-PROD-STATUS = '00' AND PROD-IS-LOAN
               MOVE PROD-ACCRUAL-BASIS TO LOAN-ACCRUAL-BASIS
               MOVE PROD-CATEGORY      TO LOAN-CATEGORY
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDLOAN  PRODUCT NOT FOUND/NOT LOAN '
                       LOAN-PRODUCT-CODE ' LOAN=' LOAN-NUMBER
               EXIT PARAGRAPH
           END-IF

           WRITE LOAN-RECORD
           IF WS-LOAN-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDLOAN  WRITE ERROR ' WS-LOAN-STATUS
                       ' KEY=' LOAN-NUMBER
           END-IF.

      *================================================================*
      * PASS 2 - DUE TABLE                                             *
      *================================================================*
       2000-LOAD-DUE-TABLE.
           OPEN INPUT DUE-SEED-FILE
           OPEN I-O LOAN-FILE
           IF WS-DUE-STATUS NOT = '00' OR WS-LOAN-STATUS NOT = '00'
               MOVE 'LDLOAN' TO ABND-PROGRAM
               MOVE '2000-LOAD-DUE-TABLE' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - LDUESEED/LOANMST' TO ABND-MESSAGE
               MOVE WS-DUE-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ DUE-SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-PROCESS-DUE
               END-READ
           END-PERFORM
           CLOSE DUE-SEED-FILE LOAN-FILE.

       2100-PROCESS-DUE.
           IF DUE-SEED-LINE(1:1) = '#' OR DUE-SEED-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-DUE-FIELDS
           UNSTRING DUE-SEED-LINE DELIMITED BY '|'
               INTO WS-DF(1) WS-DF(2) WS-DF(3) WS-DF(4) WS-DF(5)
           END-UNSTRING
           MOVE WS-DF(1) TO LOAN-NUMBER
           READ LOAN-FILE
           IF WS-LOAN-STATUS NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDLOAN  DUE ENTRY FOR UNKNOWN LOAN ' WS-DF(1)
               EXIT PARAGRAPH
           END-IF
           IF LOAN-DUE-COUNT NOT < 8
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDLOAN  DUE TABLE FULL ' LOAN-NUMBER
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO LOAN-DUE-COUNT
           MOVE LOAN-DUE-COUNT TO WS-N
           COMPUTE LOAN-DUE-DATE(WS-N) = FUNCTION NUMVAL(WS-DF(2))
           COMPUTE LOAN-DUE-INT(WS-N)  = FUNCTION NUMVAL(WS-DF(3))
           COMPUTE LOAN-DUE-PRIN(WS-N) = FUNCTION NUMVAL(WS-DF(4))
           MOVE WS-DF(5)(1:1) TO LOAN-DUE-FEE-FLAG(WS-N)
           PERFORM 8000-DERIVE-PAST-DUE
           REWRITE LOAN-RECORD
           IF WS-LOAN-STATUS = '00'
               ADD 1 TO WS-DUE-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDLOAN  REWRITE ERROR ' WS-LOAN-STATUS
                       ' KEY=' LOAN-NUMBER
           END-IF.

      *----------------------------------------------------------------*
      * PAST-DUE BUCKETS FROM THE DUE TABLE (DUE BEFORE BUSINESS DATE) *
      * DAYS PAST DUE IS TAKEN AS EXTRACTED (PRIOR EOD VALUE)          *
      *----------------------------------------------------------------*
       8000-DERIVE-PAST-DUE.
           MOVE ZERO TO LOAN-PAST-DUE-PRIN LOAN-PAST-DUE-INT
                        LOAN-OLDEST-DUE-DATE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LOAN-DUE-COUNT
               IF LOAN-DUE-DATE(WS-I) < CTL-BUSINESS-DATE
                   ADD LOAN-DUE-PRIN(WS-I) TO LOAN-PAST-DUE-PRIN
                   ADD LOAN-DUE-INT(WS-I)  TO LOAN-PAST-DUE-INT
                   IF LOAN-OLDEST-DUE-DATE = ZERO
                       MOVE LOAN-DUE-DATE(WS-I) TO LOAN-OLDEST-DUE-DATE
                   END-IF
               END-IF
           END-PERFORM
           MOVE LOAN-LATE-FEES-OUTST TO LOAN-PAST-DUE-FEES
           INITIALIZE DELQ-PARMS
           MOVE 'AGE '              TO DLQ-FUNCTION
           MOVE LOAN-PRODUCT-CODE   TO DLQ-PRODUCT-CODE
           MOVE LOAN-CATEGORY       TO DLQ-CATEGORY
           MOVE LOAN-STATUS         TO DLQ-CURRENT-STATUS
           MOVE LOAN-DAYS-PAST-DUE  TO DLQ-DAYS-PAST-DUE
                                       DLQ-PRIOR-DPD
           CALL 'DELQBKT' USING DELQ-PARMS
           IF DLQ-OK
               MOVE DLQ-BUCKET      TO LOAN-DELQ-BUCKET
               MOVE DLQ-BUREAU-FLAG TO LOAN-BUREAU-RPT-FLAG
           END-IF.
