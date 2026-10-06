      *================================================================*
      * PROGRAM  : ELIGCHK                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DEPOSIT PRODUCT ELIGIBILITY FOR ACCOUNT OPENING.    *
      *            EVALUATES EVERY RULE AND RETURNS ALL APPLICABLE     *
      *            DECLINE REASON CODES (SEE ELIGPARM FOR THE LIST).   *
      *            CALLED BY ACCTOPEN (CB03) AND THE ONLINE ACCOUNT    *
      *            OPENING API BRIDGE.                                 *
      * LINKAGE  : ELIGPARM                                            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-01-20 BHS  ORIGINAL                                       *
      * 2003-06-11 BHS  OFAC MUST BE CLEAR BEFORE OPENING              *
      * 2009-11-17 DPS  RETIRED PRODUCTS NOT OPENABLE (SVST)           *
      * 2012-02-27 DPS  CUSTODIAL ACCOUNTS DISCONTINUED - MIN AGE 18   *
      * 2017-02-08 AKW  HYSV: MAX 5 PER CUSTOMER, KYC-PENDING OPENING  *
      *                 ALLOWED UP TO 10,000.00 (DIGITAL ONBOARDING)   *
      * 2018-05-11 AKW  FINCEN CDD - REVIEW-OVERDUE CHECK (E14)        *
      * 2020-08-03 JLM  RETURN ALL DECLINE REASONS (CHG1960)           *
      * 2023-04-17 TKO  CORPORATE PRODUCTS REQUIRE CB/IN SEGMENT       *
      *                 (INC19876 - RETAIL SOLE PROP OPENED COPR)      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELIGCHK.
       AUTHOR. B H SANDOVAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DATEPARM.

       01  WS-RULE-VALUES.
           05  WS-MIN-AGE                  PIC 9(03) VALUE 018.
           05  WS-HYSV-MAX-ACCOUNTS        PIC 9(03) VALUE 005.
           05  WS-KYC-PENDING-MAX-AMT      PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
      *    2017-02 AKW WAS 5000.00 DURING PILOT
      *    05  WS-KYC-PENDING-MAX-AMT      PIC S9(13)V99 COMP-3
      *                                    VALUE 5000.00.

       01  WS-AGE-WORK.
           05  WS-BIRTH-DATE               PIC 9(08).
           05  WS-BIRTH-R REDEFINES WS-BIRTH-DATE.
               10  WS-BIRTH-CCYY           PIC 9(04).
               10  WS-BIRTH-MMDD           PIC 9(04).
           05  WS-ASOF-DATE                PIC 9(08).
           05  WS-ASOF-R REDEFINES WS-ASOF-DATE.
               10  WS-ASOF-CCYY            PIC 9(04).
               10  WS-ASOF-MMDD            PIC 9(04).
           05  WS-AGE                      PIC S9(04).

       LINKAGE SECTION.
       COPY ELIGPARM.

       PROCEDURE DIVISION USING ELIGCHK-PARMS.
       0000-MAIN.
           INITIALIZE ELG-OUTPUT
           MOVE 00 TO ELG-RETURN-CODE
           INITIALIZE DATE-PARMS
           SET DTU-VALIDATE TO TRUE
           MOVE ELG-AS-OF-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-RESULT-YES
               MOVE 12  TO ELG-RETURN-CODE
               MOVE 'N' TO ELG-ELIGIBLE-FLAG
               GOBACK
           END-IF

           PERFORM 1000-PRODUCT-RULES
           PERFORM 2000-CUSTOMER-STATUS-RULES
           PERFORM 3000-KYC-RULES
           PERFORM 4000-AMOUNT-RULES
           PERFORM 5000-AGE-RULE
           PERFORM 6000-COUNT-RULES
           PERFORM 7000-SCREENING-RULES

           IF ELG-DECLINE-COUNT = 0
               MOVE 'Y' TO ELG-ELIGIBLE-FLAG
               MOVE 00  TO ELG-RETURN-CODE
           ELSE
               MOVE 'N' TO ELG-ELIGIBLE-FLAG
               MOVE 08  TO ELG-RETURN-CODE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
      * PRODUCT: DEPOSIT ONLY, ACTIVE AND EFFECTIVE, CUSTOMER TYPE.    *
      * PRODUCT TYPE I = INDIVIDUALS ONLY, C = CORPORATE ONLY,         *
      * B = ALL (INCLUDING TRUSTS). TRUSTS MAY OPEN 'B' PRODUCTS ONLY. *
      *----------------------------------------------------------------*
       1000-PRODUCT-RULES.
           IF ELG-PROD-CATEGORY NOT = 'SV' AND NOT = 'DD'
              AND NOT = 'TD' AND NOT = 'MM'
               MOVE 'E01' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF
           IF ELG-PROD-STATUS NOT = 'A'
              OR ELG-PROD-EFF-DATE > ELG-AS-OF-DATE
               PERFORM 8000-ADD-E02
           END-IF
           EVALUATE TRUE
               WHEN ELG-PROD-CUST-TYPE = 'B'
                   CONTINUE
               WHEN ELG-PROD-CUST-TYPE = 'I' AND ELG-CUST-TYPE = 'I'
                   CONTINUE
               WHEN ELG-PROD-CUST-TYPE = 'C' AND ELG-CUST-TYPE = 'C'
                   IF ELG-SEGMENT NOT = 'CB' AND NOT = 'IN'
                       MOVE 'E10' TO
                            ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
                       ADD 1 TO ELG-DECLINE-COUNT
                   END-IF
               WHEN OTHER
                   MOVE 'E03' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
                   ADD 1 TO ELG-DECLINE-COUNT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACTIVE CUSTOMERS AND PROSPECTS (ONBOARDING) MAY OPEN ACCOUNTS  *
      *----------------------------------------------------------------*
       2000-CUSTOMER-STATUS-RULES.
           IF ELG-CUST-STATUS NOT = 'A' AND NOT = 'P'
               MOVE 'E04' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KYC: VERIFIED REQUIRED. PENDING IS PERMITTED ONLY FOR HYSV     *
      * WITH AN OPENING DEPOSIT NOT EXCEEDING 10,000.00. A VERIFIED    *
      * CUSTOMER WHOSE REVIEW DATE HAS PASSED IS OVERDUE (E14) EVEN IF *
      * MONTH-END KYCREVW HAS NOT YET SET THE STATUS TO EXPIRED.       *
      *----------------------------------------------------------------*
       3000-KYC-RULES.
           EVALUATE ELG-KYC-STATUS
               WHEN 'V'
                   IF ELG-KYC-REVIEW-DATE NOT = ZERO
                      AND ELG-KYC-REVIEW-DATE < ELG-AS-OF-DATE
                       MOVE 'E14' TO
                            ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
                       ADD 1 TO ELG-DECLINE-COUNT
                   END-IF
               WHEN 'P'
                   IF ELG-PRODUCT-CODE NOT = 'HYSV'
                      OR ELG-OPEN-AMOUNT > WS-KYC-PENDING-MAX-AMT
                       MOVE 'E06' TO
                            ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
                       ADD 1 TO ELG-DECLINE-COUNT
                   END-IF
               WHEN OTHER
      *            E EXPIRED, F FAILED, R EDD OUTSTANDING
                   MOVE 'E05' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
                   ADD 1 TO ELG-DECLINE-COUNT
           END-EVALUATE.

      *----------------------------------------------------------------*
       4000-AMOUNT-RULES.
           IF ELG-OPEN-AMOUNT < ELG-PROD-MIN-OPEN-AMT
               MOVE 'E07' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INDIVIDUALS MUST BE 18 OR OLDER ON THE AS-OF DATE. AGE IS IN   *
      * COMPLETED YEARS (BIRTHDAY MUST HAVE BEEN REACHED).             *
      *----------------------------------------------------------------*
       5000-AGE-RULE.
           IF ELG-CUST-TYPE NOT = 'I'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-VALIDATE TO TRUE
           MOVE ELG-BIRTH-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-RESULT-YES OR ELG-BIRTH-DATE > ELG-AS-OF-DATE
               MOVE 'E13' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE ELG-BIRTH-DATE TO WS-BIRTH-DATE
           MOVE ELG-AS-OF-DATE TO WS-ASOF-DATE
           COMPUTE WS-AGE = WS-ASOF-CCYY - WS-BIRTH-CCYY
           IF WS-ASOF-MMDD < WS-BIRTH-MMDD
               SUBTRACT 1 FROM WS-AGE
           END-IF
           MOVE WS-AGE TO ELG-CUST-AGE
           IF WS-AGE < WS-MIN-AGE
               MOVE 'E08' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PER-CUSTOMER PRODUCT LIMITS                                    *
      *----------------------------------------------------------------*
       6000-COUNT-RULES.
           IF ELG-PRODUCT-CODE = 'HYSV'
              AND ELG-EXISTING-SAME-PROD >= WS-HYSV-MAX-ACCOUNTS
               MOVE 'E09' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF.

      *----------------------------------------------------------------*
       7000-SCREENING-RULES.
           IF ELG-OFAC-STATUS NOT = 'C'
               MOVE 'E11' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF
           IF ELG-RISK-RATING = 'P'
               MOVE 'E12' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
               ADD 1 TO ELG-DECLINE-COUNT
           END-IF.

      *----------------------------------------------------------------*
       8000-ADD-E02.
           MOVE 'E02' TO ELG-DECLINE-CODE(ELG-DECLINE-COUNT + 1)
           ADD 1 TO ELG-DECLINE-COUNT.
