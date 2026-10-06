      *================================================================*
      * PROGRAM  : KYCTIER                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DETERMINES THE CUSTOMER DUE DILIGENCE (KYC) TIER    *
      *            1 = SIMPLIFIED, 2 = STANDARD, 3 = ENHANCED, WHETHER *
      *            ENHANCED DUE DILIGENCE (EDD) IS REQUIRED, AND THE   *
      *            NEXT PERIODIC KYC REVIEW DATE.                      *
      *            CALLED BY KYCREVW (BATCH), CUSTMNT AND ACCTOPEN     *
      *            (ONLINE).                                           *
      * LINKAGE  : KYCTPARM                                            *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - CIP RISK TIERS (USA PATRIOT 326)    *
      * 2006-08-30 BHS  PEP ALWAYS ENHANCED (SEC 312 GUIDANCE)         *
      * 2009-04-02 DPS  GOVERNMENT ENTITIES SIMPLIFIED (CHG0217)       *
      * 2011-02-01 DPS  HIGH-RISK JURISDICTION TABLE MOVED TO CIFRSKTB *
      * 2014-03-12 DPS  REMOVED 5-YEAR TIER 1 CYCLE - SEE CHG1187      *
      * 2018-05-11 AKW  FINCEN CDD RULE - TRUSTS ENHANCED (BENEFICIAL  *
      *                 OWNERSHIP), REVIEW CYCLE BY TIER               *
      * 2019-10-07 AKW  MSB ALWAYS EDD (CHG1844)                       *
      * 2021-03-22 JLM  TIER 1 INCOME CEILING 25000 -> 30000 (CHG2077) *
      * 2022-06-15 JLM  REASON CODE OUTPUT (CHG2391)                   *
      * 2023-09-18 TKO  INC20331 - OFAC POTENTIAL HIT FORCES EDD       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KYCTIER.
       AUTHOR. B H SANDOVAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CIFRSKTB.
       COPY DATEPARM.

       01  WS-FLAGS.
           05  WS-HR-COUNTRY-FLAG          PIC X(01).
               88  WS-HIGH-RISK-COUNTRY              VALUE 'Y'.
           05  WS-HR-INDUSTRY-FLAG         PIC X(01).
               88  WS-HIGH-RISK-INDUSTRY             VALUE 'Y'.
           05  WS-CASH-INT-FLAG            PIC X(01).
               88  WS-CASH-INTENSIVE                 VALUE 'Y'.

      *--- REVIEW CYCLE IN MONTHS BY TIER ----------------------------*
       01  WS-REVIEW-CYCLE-VALUES.
           05  FILLER                      PIC 9(03) VALUE 036.
           05  FILLER                      PIC 9(03) VALUE 024.
           05  FILLER                      PIC 9(03) VALUE 012.
       01  WS-REVIEW-CYCLE-TABLE REDEFINES WS-REVIEW-CYCLE-VALUES.
           05  WS-REVIEW-MONTHS            PIC 9(03) OCCURS 3 TIMES.
      *    2014-03 DPS REMOVED - SEE CHG1187
      *    05  FILLER                      PIC 9(03) VALUE 060.

       01  WS-PEP-REVIEW-MONTHS            PIC 9(03) VALUE 012.
       01  WS-TIER1-INCOME-CEILING         PIC S9(13)V99 COMP-3
                                           VALUE 30000.00.
       01  WS-TIER-SUB                     PIC 9(01).

       LINKAGE SECTION.
       COPY KYCTPARM.

       PROCEDURE DIVISION USING KYCTIER-PARMS.
       0000-MAIN.
           INITIALIZE KYT-OUTPUT
           MOVE 00 TO KYT-RETURN-CODE
           PERFORM 1000-VALIDATE-INPUT
           IF KYT-RETURN-CODE = 12
               GOBACK
           END-IF
           PERFORM 2000-LOOKUP-TABLES
           PERFORM 3000-DETERMINE-TIER
           PERFORM 4000-NEXT-REVIEW-DATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-VALIDATE-INPUT.
           IF KYT-CUST-TYPE NOT = 'I' AND NOT = 'C' AND NOT = 'T'
               MOVE 12 TO KYT-RETURN-CODE
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-VALIDATE TO TRUE
           MOVE KYT-AS-OF-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-RESULT-YES
               MOVE 12 TO KYT-RETURN-CODE
           END-IF
           IF KYT-COUNTRY = SPACES
               MOVE 'US' TO KYT-COUNTRY
           END-IF.

      *----------------------------------------------------------------*
       2000-LOOKUP-TABLES.
           MOVE 'N' TO WS-HR-COUNTRY-FLAG
                       WS-HR-INDUSTRY-FLAG
                       WS-CASH-INT-FLAG
           SET CIF-HRC-IDX TO 1
           SEARCH CIF-HR-CTRY
               AT END CONTINUE
               WHEN CIF-HR-CTRY(CIF-HRC-IDX) = KYT-COUNTRY
                   SET WS-HIGH-RISK-COUNTRY TO TRUE
           END-SEARCH
           SET CIF-HRI-IDX TO 1
           SEARCH CIF-HR-IND
               AT END CONTINUE
               WHEN CIF-HR-IND(CIF-HRI-IDX) = KYT-INDUSTRY-CODE
                   SET WS-HIGH-RISK-INDUSTRY TO TRUE
           END-SEARCH
           SET CIF-CII-IDX TO 1
           SEARCH CIF-CI-IND
               AT END CONTINUE
               WHEN CIF-CI-IND(CIF-CII-IDX) = KYT-INDUSTRY-CODE
                   SET WS-CASH-INTENSIVE TO TRUE
           END-SEARCH.

      *----------------------------------------------------------------*
      * TIER DECISION TABLE - FIRST MATCHING ROW WINS                  *
      *----------------------------------------------------------------*
       3000-DETERMINE-TIER.
           MOVE 'N' TO KYT-EDD-REQUIRED
           EVALUATE TRUE
      *        CONFIRMED OFAC MATCH - RELATIONSHIP MAY NOT BE MAINTAINED
               WHEN KYT-OFAC-STATUS = 'R'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'OFAC' TO KYT-REASON-CODE
                   MOVE 08     TO KYT-RETURN-CODE
      *        ---- TIER 3 WITH EDD -----------------------------------
               WHEN KYT-PEP-FLAG = 'Y'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'PEPX' TO KYT-REASON-CODE
               WHEN KYT-MSB-FLAG = 'Y'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'MSBX' TO KYT-REASON-CODE
               WHEN KYT-RISK-RATING = 'H' OR KYT-RISK-RATING = 'P'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'HRSK' TO KYT-REASON-CODE
               WHEN KYT-OFAC-STATUS = 'H'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'OFAC' TO KYT-REASON-CODE
               WHEN WS-HIGH-RISK-COUNTRY
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'CNTY' TO KYT-REASON-CODE
               WHEN WS-HIGH-RISK-INDUSTRY
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'Y'    TO KYT-EDD-REQUIRED
                   MOVE 'INDH' TO KYT-REASON-CODE
      *        ---- TIER 3 WITHOUT EDD --------------------------------
               WHEN KYT-CUST-TYPE = 'T'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'TRST' TO KYT-REASON-CODE
               WHEN KYT-SEGMENT = 'PW'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'PWLT' TO KYT-REASON-CODE
               WHEN KYT-CUST-TYPE = 'C' AND WS-CASH-INTENSIVE
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'CASH' TO KYT-REASON-CODE
               WHEN KYT-CUST-TYPE = 'I' AND KYT-COUNTRY NOT = 'US'
                   MOVE '3'    TO KYT-RESULT-TIER
                   MOVE 'FRGN' TO KYT-REASON-CODE
      *        ---- TIER 1 SIMPLIFIED ---------------------------------
      *        PUBLIC ADMINISTRATION (NAICS 92) IN INSTITUTIONAL SEG
               WHEN KYT-CUST-TYPE = 'C' AND KYT-SEGMENT = 'IN'
                    AND KYT-INDUSTRY-CODE(1:2) = '92'
                   MOVE '1'    TO KYT-RESULT-TIER
                   MOVE 'GOVT' TO KYT-REASON-CODE
               WHEN KYT-CUST-TYPE = 'I' AND KYT-SEGMENT = 'RT'
                    AND KYT-RISK-RATING = 'L'
                    AND KYT-ANNUAL-INCOME < WS-TIER1-INCOME-CEILING
                   MOVE '1'    TO KYT-RESULT-TIER
                   MOVE 'LOWI' TO KYT-REASON-CODE
      *        ---- EVERYONE ELSE -------------------------------------
               WHEN OTHER
                   MOVE '2'    TO KYT-RESULT-TIER
                   MOVE 'STDX' TO KYT-REASON-CODE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NEXT REVIEW = AS-OF + CYCLE MONTHS. PEPS ARE ALWAYS REVIEWED   *
      * ANNUALLY REGARDLESS OF TIER.                                   *
      *----------------------------------------------------------------*
       4000-NEXT-REVIEW-DATE.
           MOVE KYT-RESULT-TIER TO WS-TIER-SUB
           MOVE WS-REVIEW-MONTHS(WS-TIER-SUB) TO KYT-REVIEW-MONTHS
           IF KYT-PEP-FLAG = 'Y'
              AND KYT-REVIEW-MONTHS > WS-PEP-REVIEW-MONTHS
               MOVE WS-PEP-REVIEW-MONTHS TO KYT-REVIEW-MONTHS
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE KYT-AS-OF-DATE    TO DTU-DATE-1
           MOVE KYT-REVIEW-MONTHS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO KYT-NEXT-REVIEW-DATE.
