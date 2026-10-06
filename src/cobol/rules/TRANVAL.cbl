      *================================================================*
      * PROGRAM  : TRANVAL                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EDITS ONE MONETARY TRANSACTION AGAINST THE ACCOUNT  *
      *            AND PRODUCT. USED BY BATCH EDIT (TRANEDIT) AND BY   *
      *            THE ONLINE TRANSFER / TELLER TRANSACTIONS.          *
      *            EDITS ARE APPLIED IN THIS ORDER, FIRST FAILURE WINS:*
      *              1 TRAN CODE   2 AMOUNT    3 ACCOUNT  4 PRODUCT    *
      *              5 EFF DATE    6 CHANNEL   7 ACCT STATUS           *
      *              8 TIME DEPOSIT 9 CHECKS  10 DAILY WD LIMIT        *
      * LINKAGE  : TRVPARM (REJECT CODES DOCUMENTED THERE)             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1993-04-19 RJT  DORMANT ACCOUNT HANDLING                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  CASH FLAG PASSED BACK FOR CTR                  *
      * 2004-02-17 BHS  BACK-DATING LIMIT 30 DAYS (WAS 90) INC10442    *
      * 2007-05-14 BHS  CHANNEL / TRAN CODE MATRIX, DAILY WD LIMIT     *
      * 2011-08-30 DPS  FROZEN - ALLOW DREV ONLY (CHG0987)             *
      * 2014-01-06 DPS  ESCHEATED / CHARGED-OFF STATES                 *
      * 2015-10-02 DPS  CORE CONVERSION - PENDING OPEN STATE           *
      * 2016-09-23 DPS  SAME-DAY ACH CHANNEL ADDED TO MATRIX           *
      * 2018-02-20 AKW  CD GRACE PERIOD DEPOSITS (CHG2210)             *
      * 2019-03-25 AKW  RESTRICTED - BANK-ORIGINATED DEBITS ALLOWED    *
      * 2021-11-08 AKW  FUTURE DATING 5 BUSINESS DAYS (WAS 10 CAL)     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANVAL.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TRANCODE.
       COPY DATEPARM.

       01  WS-LIMITS.
           05  WS-MAX-FUTURE-BUS-DAYS      PIC S9(05) VALUE +5.
      *    2004-02 BHS WAS 90 - SEE INC10442
           05  WS-MAX-BACK-CAL-DAYS        PIC S9(05) VALUE +30.

       01  WS-WORK.
           05  WS-MAX-FUTURE-DATE          PIC 9(08).
           05  WS-DAYS-BACK                PIC S9(07).
           05  WS-PROJECTED-WD             PIC S9(11)V99 COMP-3.
           05  WS-CHAN-OK                  PIC X(01).
           05  WS-CHAN-SUB                 PIC 9(02).
           05  WS-TD-CODE-OK               PIC X(01).

      *----------------------------------------------------------------*
      * CHANNEL / TRANSACTION CODE COMPATIBILITY MATRIX                *
      *   CODE(4) + UP TO 8 PERMITTED CHANNELS (3 EACH). '***' = ANY.  *
      *----------------------------------------------------------------*
       01  WS-CHANNEL-MATRIX-VALUES.
           05  FILLER PIC X(28) VALUE 'DCSHBRNATM                  '.
           05  FILLER PIC X(28) VALUE 'DCHKBRNATMMOBBAT            '.
           05  FILLER PIC X(28) VALUE 'DACHACHSDABAT               '.
           05  FILLER PIC X(28) VALUE 'DWIRWIRBATBRN               '.
           05  FILLER PIC X(28) VALUE 'DXFR***                     '.
           05  FILLER PIC X(28) VALUE 'DINTBAT                     '.
           05  FILLER PIC X(28) VALUE 'DREVBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'DLONBATBRNONL               '.
           05  FILLER PIC X(28) VALUE 'DSWPBAT                     '.
           05  FILLER PIC X(28) VALUE 'DRFDBATBRNONL               '.
           05  FILLER PIC X(28) VALUE 'DCOFBAT                     '.
           05  FILLER PIC X(28) VALUE 'ADJCBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'DMATBAT                     '.
           05  FILLER PIC X(28) VALUE 'WCSHBRNATM                  '.
           05  FILLER PIC X(28) VALUE 'WATMATM                     '.
           05  FILLER PIC X(28) VALUE 'WCHKBAT                     '.
           05  FILLER PIC X(28) VALUE 'WACHACHSDABAT               '.
           05  FILLER PIC X(28) VALUE 'WWIRWIRBRNONLAPIBAT         '.
           05  FILLER PIC X(28) VALUE 'WXFR***                     '.
           05  FILLER PIC X(28) VALUE 'WPOSPOS                     '.
           05  FILLER PIC X(28) VALUE 'WLNP***                     '.
           05  FILLER PIC X(28) VALUE 'WSWPBAT                     '.
           05  FILLER PIC X(28) VALUE 'WPENBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'WBWHBAT                     '.
           05  FILLER PIC X(28) VALUE 'WMATBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'WESCBAT                     '.
           05  FILLER PIC X(28) VALUE 'ADJDBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'WFODBAT                     '.
           05  FILLER PIC X(28) VALUE 'WFNSBAT                     '.
           05  FILLER PIC X(28) VALUE 'WFMNBATBRN                  '.
           05  FILLER PIC X(28) VALUE 'WFWRBATBRNWIRONL            '.
           05  FILLER PIC X(28) VALUE 'WFANBAT                     '.
           05  FILLER PIC X(28) VALUE 'WFRDBAT                     '.
           05  FILLER PIC X(28) VALUE 'WFLTBAT                     '.
           05  FILLER PIC X(28) VALUE 'WFOTBATBRNONL               '.
       01  WS-CHANNEL-MATRIX REDEFINES WS-CHANNEL-MATRIX-VALUES.
           05  CM-ENTRY OCCURS 35 TIMES INDEXED BY CM-IDX.
               10  CM-CODE                 PIC X(04).
               10  CM-CHANNEL              PIC X(03) OCCURS 8 TIMES.

      *----------------------------------------------------------------*
      * TIME DEPOSIT - THE ONLY CODES A CD WILL ACCEPT                 *
      *----------------------------------------------------------------*
       01  WS-TD-DEBIT-CODES               PIC X(16)
                                           VALUE 'WPENWMATWBWHADJD'.
       01  WS-TD-DEBIT-R REDEFINES WS-TD-DEBIT-CODES.
           05  WS-TD-DEBIT-CODE            PIC X(04) OCCURS 4 TIMES.
       01  WS-TD-CREDIT-CODES              PIC X(16)
                                           VALUE 'DINTDMATDREVADJC'.
       01  WS-TD-CREDIT-R REDEFINES WS-TD-CREDIT-CODES.
           05  WS-TD-CREDIT-CODE           PIC X(04) OCCURS 4 TIMES.
       01  WS-TD-SUB                       PIC 9(01).

       LINKAGE SECTION.
       COPY TRVPARM.

       PROCEDURE DIVISION USING TRANVAL-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-EDIT-TRAN-CODE
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2100-EDIT-AMOUNT
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2200-EDIT-ACCOUNT-PRODUCT
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2300-EDIT-EFFECTIVE-DATE
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2400-EDIT-CHANNEL
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2500-EDIT-ACCOUNT-STATUS
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2600-EDIT-TIME-DEPOSIT
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2700-EDIT-CHECKS
           END-IF
           IF TRV-REJECT-CODE = SPACES
               PERFORM 2800-EDIT-DAILY-LIMIT
           END-IF
           IF TRV-REJECT-CODE = SPACES
               MOVE 00 TO TRV-RETURN-CODE
           ELSE
               IF TRV-RETURN-CODE = 00
                   MOVE 08 TO TRV-RETURN-CODE
               END-IF
           END-IF
           GOBACK.

       1000-INITIALISE.
           MOVE SPACES TO TRV-DR-CR TRV-CASH-FLAG TRV-REGD-FLAG
                          TRV-CUST-INIT-FLAG TRV-REJECT-CODE
                          TRV-REJECT-TEXT
           MOVE 'N'    TO TRV-REACTIVATE-FLAG TRV-WD-LIMIT-FLAG
           MOVE 0      TO TRV-POST-PRIORITY
           MOVE 00     TO TRV-RETURN-CODE.

      *----------------------------------------------------------------*
      * 1. TRANSACTION CODE MUST BE IN THE TRANCODE TABLE              *
      *----------------------------------------------------------------*
       2000-EDIT-TRAN-CODE.
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END
                   MOVE 'TCIN' TO TRV-REJECT-CODE
                   MOVE 'INVALID TRANSACTION CODE' TO TRV-REJECT-TEXT
               WHEN TC-CODE(TC-IDX) = TRV-TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)          TO TRV-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX)      TO TRV-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX)      TO TRV-REGD-FLAG
                   MOVE TC-CUST-INIT-FLAG(TC-IDX) TO TRV-CUST-INIT-FLAG
                   MOVE TC-POST-PRIORITY(TC-IDX)  TO TRV-POST-PRIORITY
           END-SEARCH.

      *----------------------------------------------------------------*
      * 2. AMOUNT MUST BE GREATER THAN ZERO (DIRECTION IS IN DR/CR)    *
      *----------------------------------------------------------------*
       2100-EDIT-AMOUNT.
           IF TRV-AMOUNT NOT > 0
               MOVE 'AMT0' TO TRV-REJECT-CODE
               MOVE 'AMOUNT MUST BE GREATER THAN ZERO'
                 TO TRV-REJECT-TEXT
           END-IF.

      *----------------------------------------------------------------*
      * 3/4. ACCOUNT AND PRODUCT MUST EXIST                            *
      *----------------------------------------------------------------*
       2200-EDIT-ACCOUNT-PRODUCT.
           EVALUATE TRUE
               WHEN NOT TRV-ACCT-FOUND
                   MOVE 'ACNF' TO TRV-REJECT-CODE
                   MOVE 'ACCOUNT NOT ON FILE' TO TRV-REJECT-TEXT
               WHEN NOT TRV-PROD-FOUND
                   MOVE 'PRNF' TO TRV-REJECT-CODE
                   MOVE 'PRODUCT NOT ON FILE' TO TRV-REJECT-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 5. EFFECTIVE DATE                                              *
      *    - MUST BE A VALID DATE                                      *
      *    - NOT MORE THAN 5 BUSINESS DAYS AFTER THE BUSINESS DATE     *
      *    - NOT MORE THAN 30 CALENDAR DAYS BEFORE THE BUSINESS DATE,  *
      *      EXCEPT ADJUSTMENTS (ADJC / ADJD) WHICH MAY BE BACK-VALUED *
      *----------------------------------------------------------------*
       2300-EDIT-EFFECTIVE-DATE.
           INITIALIZE DATE-PARMS
           SET DTU-VALIDATE TO TRUE
           MOVE TRV-EFFECTIVE-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-RESULT-YES
               MOVE 'EDIV' TO TRV-REJECT-CODE
               MOVE 'EFFECTIVE DATE INVALID' TO TRV-REJECT-TEXT
               EXIT PARAGRAPH
           END-IF

           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE TRV-BUSINESS-DATE      TO DTU-DATE-1
           MOVE WS-MAX-FUTURE-BUS-DAYS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-OK
               MOVE 12 TO TRV-RETURN-CODE
               MOVE 'EDIV' TO TRV-REJECT-CODE
               MOVE 'BUSINESS DATE INVALID' TO TRV-REJECT-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE DTU-RESULT-DATE TO WS-MAX-FUTURE-DATE
           IF TRV-EFFECTIVE-DATE > WS-MAX-FUTURE-DATE
               MOVE 'EDFT' TO TRV-REJECT-CODE
               MOVE 'EFFECTIVE DATE TOO FAR IN FUTURE'
                 TO TRV-REJECT-TEXT
               EXIT PARAGRAPH
           END-IF

           IF TRV-TRAN-CODE(1:3) = 'ADJ'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE TRV-EFFECTIVE-DATE TO DTU-DATE-1
           MOVE TRV-BUSINESS-DATE  TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS-BACK
           IF WS-DAYS-BACK > WS-MAX-BACK-CAL-DAYS
               MOVE 'EDBK' TO TRV-REJECT-CODE
               MOVE 'EFFECTIVE DATE BACK-DATED OVER 30 DAYS'
                 TO TRV-REJECT-TEXT
           END-IF.

      *----------------------------------------------------------------*
      * 6. CHANNEL MUST BE PERMITTED FOR THE TRAN CODE                 *
      *    E.G. WCSH ONLY BRN/ATM, WATM ONLY ATM, WCHK ONLY BAT,       *
      *    WPOS ONLY POS. A CODE MISSING FROM THE MATRIX IS ALLOWED.   *
      *----------------------------------------------------------------*
       2400-EDIT-CHANNEL.
           MOVE 'Y' TO WS-CHAN-OK
           SET CM-IDX TO 1
           SEARCH CM-ENTRY
               AT END
                   CONTINUE
               WHEN CM-CODE(CM-IDX) = TRV-TRAN-CODE
                   MOVE 'N' TO WS-CHAN-OK
                   PERFORM VARYING WS-CHAN-SUB FROM 1 BY 1
                           UNTIL WS-CHAN-SUB > 8
                       IF CM-CHANNEL(CM-IDX, WS-CHAN-SUB) = '***'
                          OR CM-CHANNEL(CM-IDX, WS-CHAN-SUB)
                             = TRV-CHANNEL
                           MOVE 'Y' TO WS-CHAN-OK
                       END-IF
                   END-PERFORM
           END-SEARCH
           IF WS-CHAN-OK = 'N'
               MOVE 'CHNL' TO TRV-REJECT-CODE
               STRING 'CODE ' TRV-TRAN-CODE ' NOT ALLOWED ON CHANNEL '
                      TRV-CHANNEL DELIMITED BY SIZE
                 INTO TRV-REJECT-TEXT
           END-IF.

      *----------------------------------------------------------------*
      * 7. ACCOUNT STATUS RULES                                        *
      *   A ACTIVE        ALL                                          *
      *   C CLOSED        NONE                                         *
      *   F FROZEN        DREV ONLY (DECEASED / COURT ORDER)           *
      *   R RESTRICTED    CREDITS; BANK-ORIGINATED DEBITS ONLY         *
      *   D DORMANT       CREDITS (CUSTOMER CREDIT REACTIVATES);       *
      *                   DEBITS ONLY AT BRANCH WITH ID VERIFIED       *
      *   E ESCHEATED     NONE                                         *
      *   X CHARGED OFF   CREDITS ONLY (RECOVERIES)                    *
      *   P PENDING OPEN  CREDITS ONLY (INITIAL FUNDING)               *
      *----------------------------------------------------------------*
       2500-EDIT-ACCOUNT-STATUS.
           EVALUATE TRUE
               WHEN TRV-ACCT-STATUS = 'A'
                   CONTINUE
               WHEN TRV-ACCT-STATUS = 'C'
                   MOVE 'ACLS' TO TRV-REJECT-CODE
                   MOVE 'ACCOUNT CLOSED' TO TRV-REJECT-TEXT
               WHEN TRV-ACCT-STATUS = 'F'
                   IF TRV-TRAN-CODE NOT = 'DREV'
                       MOVE 'AFRZ' TO TRV-REJECT-CODE
                       MOVE 'ACCOUNT FROZEN' TO TRV-REJECT-TEXT
                   END-IF
               WHEN TRV-ACCT-STATUS = 'R'
                   IF TRV-DR-CR = 'D' AND TRV-CUST-INIT-FLAG = 'Y'
                       MOVE 'ARST' TO TRV-REJECT-CODE
                       MOVE 'ACCOUNT RESTRICTED - NO DEBITS'
                         TO TRV-REJECT-TEXT
                   END-IF
               WHEN TRV-ACCT-STATUS = 'D'
                   PERFORM 2510-DORMANT-RULES
               WHEN TRV-ACCT-STATUS = 'E'
                   MOVE 'AESC' TO TRV-REJECT-CODE
                   MOVE 'ACCOUNT ESCHEATED TO STATE' TO TRV-REJECT-TEXT
               WHEN TRV-ACCT-STATUS = 'X'
                   IF TRV-DR-CR = 'D'
                       MOVE 'ACOF' TO TRV-REJECT-CODE
                       MOVE 'ACCOUNT CHARGED OFF - CREDITS ONLY'
                         TO TRV-REJECT-TEXT
                   END-IF
               WHEN TRV-ACCT-STATUS = 'P'
                   IF TRV-DR-CR = 'D'
                       MOVE 'APND' TO TRV-REJECT-CODE
                       MOVE 'ACCOUNT PENDING OPEN - CREDITS ONLY'
                         TO TRV-REJECT-TEXT
                   END-IF
               WHEN OTHER
                   MOVE 'ACLS' TO TRV-REJECT-CODE
                   MOVE 'ACCOUNT STATUS UNKNOWN' TO TRV-REJECT-TEXT
           END-EVALUATE.

       2510-DORMANT-RULES.
           IF TRV-DR-CR = 'C'
      *        ONLY A CUSTOMER-INITIATED CREDIT WAKES THE ACCOUNT UP;
      *        INTEREST AND OTHER BANK CREDITS DO NOT (STATE UNCLAIMED
      *        PROPERTY RULES - OWNER-GENERATED ACTIVITY ONLY)
               IF TRV-CUST-INIT-FLAG = 'Y'
                   MOVE 'Y' TO TRV-REACTIVATE-FLAG
               END-IF
           ELSE
               IF TRV-CHANNEL = 'BRN' AND TRV-ID-VERIFIED-FLAG = 'Y'
                   MOVE 'Y' TO TRV-REACTIVATE-FLAG
               ELSE
                   IF TRV-CUST-INIT-FLAG = 'Y'
                       MOVE 'ADRM' TO TRV-REJECT-CODE
                       MOVE 'DORMANT - VISIT BRANCH WITH ID'
                         TO TRV-REJECT-TEXT
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 8. TIME DEPOSITS (CATEGORY TD)                                 *
      *    DEBITS: ONLY WPEN, WMAT, WBWH, ADJD. A WXFR IS PERMITTED    *
      *            FROM MATURITY DATE THROUGH THE GRACE PERIOD END.    *
      *    CREDITS: ONLY DINT, DMAT, DREV, ADJC, EXCEPT ADDITIONS ARE  *
      *            ACCEPTED DURING THE GRACE PERIOD (CHG2210).         *
      *----------------------------------------------------------------*
       2600-EDIT-TIME-DEPOSIT.
           IF TRV-ACCT-CATEGORY NOT = 'TD'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-TD-CODE-OK
           IF TRV-DR-CR = 'D'
               PERFORM VARYING WS-TD-SUB FROM 1 BY 1
                       UNTIL WS-TD-SUB > 4
                   IF WS-TD-DEBIT-CODE(WS-TD-SUB) = TRV-TRAN-CODE
                       MOVE 'Y' TO WS-TD-CODE-OK
                   END-IF
               END-PERFORM
               IF WS-TD-CODE-OK = 'N'
                  AND TRV-TRAN-CODE = 'WXFR'
                  AND TRV-MATURITY-DATE > 0
                  AND TRV-BUSINESS-DATE NOT < TRV-MATURITY-DATE
                  AND TRV-BUSINESS-DATE NOT > TRV-GRACE-END-DATE
                   MOVE 'Y' TO WS-TD-CODE-OK
               END-IF
               IF WS-TD-CODE-OK = 'N'
                   MOVE 'TDDR' TO TRV-REJECT-CODE
                   MOVE 'TIME DEPOSIT - DEBIT NOT PERMITTED'
                     TO TRV-REJECT-TEXT
               END-IF
           ELSE
               PERFORM VARYING WS-TD-SUB FROM 1 BY 1
                       UNTIL WS-TD-SUB > 4
                   IF WS-TD-CREDIT-CODE(WS-TD-SUB) = TRV-TRAN-CODE
                       MOVE 'Y' TO WS-TD-CODE-OK
                   END-IF
               END-PERFORM
               IF WS-TD-CODE-OK = 'N'
                  AND TRV-GRACE-END-DATE > 0
                  AND TRV-BUSINESS-DATE NOT > TRV-GRACE-END-DATE
                   MOVE 'Y' TO WS-TD-CODE-OK
               END-IF
               IF WS-TD-CODE-OK = 'N'
                   MOVE 'TDCR' TO TRV-REJECT-CODE
                   MOVE 'TIME DEPOSIT - NO ADDITIONAL DEPOSITS'
                     TO TRV-REJECT-TEXT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 9. CHECKS PAID (WCHK) REQUIRE PROD-CHECK-ALLOWED = 'Y'.        *
      *    CHECK DEPOSITS (DCHK) ARE ACCEPTED ON ANY DEPOSIT PRODUCT.  *
      *----------------------------------------------------------------*
       2700-EDIT-CHECKS.
           IF TRV-TRAN-CODE = 'WCHK'
              AND TRV-PROD-CHECK-ALLOWED NOT = 'Y'
               MOVE 'NCHK' TO TRV-REJECT-CODE
               MOVE 'CHECKS NOT PERMITTED ON THIS PRODUCT'
                 TO TRV-REJECT-TEXT
           END-IF.

      *----------------------------------------------------------------*
      * 10. DAILY ATM / CASH WITHDRAWAL LIMIT                          *
      *     APPLIES TO WATM AND TO WCSH TAKEN AT AN ATM. BRANCH CASH   *
      *     IS CONTROLLED BY TELLER OVERRIDE INSTEAD. A LIMIT OF ZERO  *
      *     MEANS NO LIMIT (CORPORATE ACCOUNTS - CNV-0117).            *
      *----------------------------------------------------------------*
       2800-EDIT-DAILY-LIMIT.
           IF TRV-TRAN-CODE = 'WATM'
              OR (TRV-TRAN-CODE = 'WCSH' AND TRV-CHANNEL = 'ATM')
               MOVE 'Y' TO TRV-WD-LIMIT-FLAG
               IF TRV-DAILY-WD-LIMIT > 0
                   COMPUTE WS-PROJECTED-WD =
                           TRV-DAILY-WD-USED + TRV-AMOUNT
                   IF WS-PROJECTED-WD > TRV-DAILY-WD-LIMIT
                       MOVE 'DWLM' TO TRV-REJECT-CODE
                       MOVE 'DAILY WITHDRAWAL LIMIT EXCEEDED'
                         TO TRV-REJECT-TEXT
                   END-IF
               END-IF
           END-IF.
