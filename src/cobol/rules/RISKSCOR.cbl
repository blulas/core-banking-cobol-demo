      *================================================================*
      * PROGRAM  : RISKSCOR                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : WEIGHTED CUSTOMER RISK SCORE (0-999) AND RATING     *
      *              L  LOW         0 - 249                            *
      *              M  MEDIUM    250 - 499                            *
      *              H  HIGH      500 - 899                            *
      *              P  PROHIBITED 900+ OR CONFIRMED OFAC MATCH        *
      *            FACTORS: ENTITY TYPE/SEGMENT, PEP, MSB, INDUSTRY,   *
      *            GEOGRAPHY, PRODUCT MIX, TENURE, CASH ACTIVITY,      *
      *            OTHER (OFAC POTENTIAL HIT, NON-SSN TAX ID, INCOME). *
      * LINKAGE  : RSKPARM                                             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - USA PATRIOT ACT                     *
      * 2006-08-30 BHS  PEP +300, OFAC POTENTIAL HIT +200              *
      * 2008-01-14 BHS  PROHIBITED THRESHOLD 950 -> 900 (REG-2008-04)  *
      * 2011-02-01 DPS  HIFCA GEOGRAPHY +40 (CHG0388)                  *
      * 2013-07-09 DPS  TENURE CREDIT FOR 10+ YEAR CUSTOMERS           *
      * 2016-05-02 DPS  PRIVATE WEALTH SEGMENT +150 (OCC MRA 2016-11)  *
      * 2019-10-07 AKW  MSB +350, FACTOR BREAKDOWN OUTPUT              *
      * 2022-11-30 JLM  ITIN / NO TAX ID +50 (CHG2455)                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RISKSCOR.
       AUTHOR. B H SANDOVAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CIFRSKTB.
       COPY DATEPARM.

      *--- FACTOR WEIGHTS ---------------------------------------------*
       01  WS-WEIGHTS.
           05  WS-W-INDIVIDUAL             PIC S9(03) VALUE +050.
           05  WS-W-CORPORATE              PIC S9(03) VALUE +100.
           05  WS-W-TRUST                  PIC S9(03) VALUE +120.
           05  WS-W-GOVT-INSTITUTIONAL     PIC S9(03) VALUE +020.
           05  WS-W-PRIVATE-WEALTH         PIC S9(03) VALUE +150.
           05  WS-W-PEP                    PIC S9(03) VALUE +300.
           05  WS-W-MSB                    PIC S9(03) VALUE +350.
           05  WS-W-HIGH-RISK-IND          PIC S9(03) VALUE +200.
           05  WS-W-CASH-INTENSIVE         PIC S9(03) VALUE +150.
           05  WS-W-HIGH-RISK-CTRY         PIC S9(03) VALUE +250.
           05  WS-W-FOREIGN                PIC S9(03) VALUE +075.
           05  WS-W-HIFCA                  PIC S9(03) VALUE +040.
           05  WS-W-DDA                    PIC S9(03) VALUE +020.
           05  WS-W-MMA                    PIC S9(03) VALUE +020.
           05  WS-W-CORP-DDA               PIC S9(03) VALUE +040.
           05  WS-W-TENURE-UNDER-1Y        PIC S9(03) VALUE +060.
           05  WS-W-TENURE-UNDER-3Y        PIC S9(03) VALUE +030.
           05  WS-W-TENURE-10Y-PLUS        PIC S9(03) VALUE -030.
           05  WS-W-CASH-HEAVY             PIC S9(03) VALUE +120.
           05  WS-W-CASH-MODERATE          PIC S9(03) VALUE +050.
           05  WS-W-OFAC-POTENTIAL         PIC S9(03) VALUE +200.
           05  WS-W-NON-SSN-TAXID          PIC S9(03) VALUE +050.
           05  WS-W-HIGH-INCOME            PIC S9(03) VALUE +040.

      *--- RATING BANDS -----------------------------------------------*
       01  WS-BAND-MEDIUM                  PIC 9(03) VALUE 250.
       01  WS-BAND-HIGH                    PIC 9(03) VALUE 500.
       01  WS-BAND-PROHIBITED              PIC 9(03) VALUE 900.
       01  WS-HIGH-INCOME-THRESHOLD        PIC S9(13)V99 COMP-3
                                           VALUE 1000000.00.

       01  WS-WORK.
           05  WS-TOTAL                    PIC S9(05) COMP-3.
           05  WS-TENURE-DAYS              PIC S9(07).
           05  WS-ZIP3                     PIC X(03).
           05  WS-FOUND                    PIC X(01).

       LINKAGE SECTION.
       COPY RSKPARM.

       PROCEDURE DIVISION USING RISKSCOR-PARMS.
       0000-MAIN.
           INITIALIZE RSK-OUTPUT
           MOVE 00 TO RSK-RETURN-CODE
           MOVE 'N' TO RSK-PROHIBITED-FLAG
           IF RSK-CUST-TYPE NOT = 'I' AND NOT = 'C' AND NOT = 'T'
               MOVE 12 TO RSK-RETURN-CODE
               GOBACK
           END-IF
           IF RSK-COUNTRY = SPACES
               MOVE 'US' TO RSK-COUNTRY
           END-IF
           PERFORM 1000-ENTITY-FACTOR
           PERFORM 1100-PEP-MSB-FACTORS
           PERFORM 1200-INDUSTRY-FACTOR
           PERFORM 1300-GEOGRAPHY-FACTOR
           PERFORM 1400-PRODUCT-FACTOR
           PERFORM 1500-TENURE-FACTOR
           PERFORM 1600-CASH-FACTOR
           PERFORM 1700-OTHER-FACTORS
           PERFORM 2000-TOTAL-AND-RATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-ENTITY-FACTOR.
           EVALUATE TRUE
               WHEN RSK-CUST-TYPE = 'I'
                   MOVE WS-W-INDIVIDUAL TO RSK-F-ENTITY
               WHEN RSK-CUST-TYPE = 'T'
                   MOVE WS-W-TRUST      TO RSK-F-ENTITY
               WHEN RSK-CUST-TYPE = 'C' AND RSK-SEGMENT = 'IN'
                   MOVE WS-W-GOVT-INSTITUTIONAL TO RSK-F-ENTITY
               WHEN OTHER
                   MOVE WS-W-CORPORATE  TO RSK-F-ENTITY
           END-EVALUATE
           IF RSK-SEGMENT = 'PW'
               ADD WS-W-PRIVATE-WEALTH TO RSK-F-ENTITY
           END-IF.

      *----------------------------------------------------------------*
       1100-PEP-MSB-FACTORS.
           IF RSK-PEP-FLAG = 'Y'
               MOVE WS-W-PEP TO RSK-F-PEP
           END-IF
           IF RSK-MSB-FLAG = 'Y'
               MOVE WS-W-MSB TO RSK-F-MSB
           END-IF.

      *----------------------------------------------------------------*
      * HIGH-RISK INDUSTRY AND CASH-INTENSIVE ARE MUTUALLY EXCLUSIVE - *
      * THE HIGHER WEIGHT APPLIES                                      *
      *----------------------------------------------------------------*
       1200-INDUSTRY-FACTOR.
           MOVE 'N' TO WS-FOUND
           SET CIF-HRI-IDX TO 1
           SEARCH CIF-HR-IND
               AT END CONTINUE
               WHEN CIF-HR-IND(CIF-HRI-IDX) = RSK-INDUSTRY-CODE
                   MOVE WS-W-HIGH-RISK-IND TO RSK-F-INDUSTRY
                   MOVE 'Y' TO WS-FOUND
           END-SEARCH
           IF WS-FOUND = 'N'
               SET CIF-CII-IDX TO 1
               SEARCH CIF-CI-IND
                   AT END CONTINUE
                   WHEN CIF-CI-IND(CIF-CII-IDX) = RSK-INDUSTRY-CODE
                       MOVE WS-W-CASH-INTENSIVE TO RSK-F-INDUSTRY
               END-SEARCH
           END-IF.

      *----------------------------------------------------------------*
       1300-GEOGRAPHY-FACTOR.
           MOVE 'N' TO WS-FOUND
           SET CIF-HRC-IDX TO 1
           SEARCH CIF-HR-CTRY
               AT END CONTINUE
               WHEN CIF-HR-CTRY(CIF-HRC-IDX) = RSK-COUNTRY
                   MOVE WS-W-HIGH-RISK-CTRY TO RSK-F-GEOGRAPHY
                   MOVE 'Y' TO WS-FOUND
           END-SEARCH
           EVALUATE TRUE
               WHEN WS-FOUND = 'Y'
                   CONTINUE
               WHEN RSK-COUNTRY NOT = 'US'
                   MOVE WS-W-FOREIGN TO RSK-F-GEOGRAPHY
               WHEN OTHER
                   MOVE RSK-ZIP(1:3) TO WS-ZIP3
                   SET CIF-HZ-IDX TO 1
                   SEARCH CIF-HIFCA-ZIP3
                       AT END CONTINUE
                       WHEN CIF-HIFCA-ZIP3(CIF-HZ-IDX) = WS-ZIP3
                           MOVE WS-W-HIFCA TO RSK-F-GEOGRAPHY
                   END-SEARCH
           END-EVALUATE.

      *----------------------------------------------------------------*
       1400-PRODUCT-FACTOR.
           IF RSK-CNT-DDA > 0
               ADD WS-W-DDA TO RSK-F-PRODUCT
           END-IF
           IF RSK-CNT-MMA > 0
               ADD WS-W-MMA TO RSK-F-PRODUCT
           END-IF
           IF RSK-CNT-CORP-DDA > 0
               ADD WS-W-CORP-DDA TO RSK-F-PRODUCT
           END-IF.

      *----------------------------------------------------------------*
      * TENURE IN DAYS: < 365 NEW, < 1095 UNDER 3 YEARS, >= 3650 10Y+  *
      *----------------------------------------------------------------*
       1500-TENURE-FACTOR.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE RSK-CUST-SINCE-DATE TO DTU-DATE-1
           MOVE RSK-AS-OF-DATE      TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-RETURN-CODE NOT = 00
      *        UNKNOWN TENURE IS TREATED AS A NEW RELATIONSHIP
               MOVE WS-W-TENURE-UNDER-1Y TO RSK-F-TENURE
           ELSE
               MOVE DTU-RESULT-NUMBER TO WS-TENURE-DAYS
               EVALUATE TRUE
                   WHEN WS-TENURE-DAYS < 365
                       MOVE WS-W-TENURE-UNDER-1Y TO RSK-F-TENURE
                   WHEN WS-TENURE-DAYS < 1095
                       MOVE WS-W-TENURE-UNDER-3Y TO RSK-F-TENURE
                   WHEN WS-TENURE-DAYS >= 3650
                       MOVE WS-W-TENURE-10Y-PLUS TO RSK-F-TENURE
                   WHEN OTHER
                       MOVE ZERO TO RSK-F-TENURE
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       1600-CASH-FACTOR.
           EVALUATE TRUE
               WHEN RSK-CASH-HEAVY
                   MOVE WS-W-CASH-HEAVY    TO RSK-F-CASH
               WHEN RSK-CASH-MODERATE
                   MOVE WS-W-CASH-MODERATE TO RSK-F-CASH
               WHEN OTHER
                   MOVE ZERO TO RSK-F-CASH
           END-EVALUATE.

      *----------------------------------------------------------------*
       1700-OTHER-FACTORS.
           IF RSK-OFAC-STATUS = 'H'
               ADD WS-W-OFAC-POTENTIAL TO RSK-F-OTHER
           END-IF
           IF RSK-TAX-ID-TYPE = 'I' OR RSK-TAX-ID-TYPE = 'N'
               ADD WS-W-NON-SSN-TAXID TO RSK-F-OTHER
           END-IF
           IF RSK-CUST-TYPE = 'I'
              AND RSK-ANNUAL-INCOME >= WS-HIGH-INCOME-THRESHOLD
               ADD WS-W-HIGH-INCOME TO RSK-F-OTHER
           END-IF.

      *----------------------------------------------------------------*
       2000-TOTAL-AND-RATE.
           COMPUTE WS-TOTAL = RSK-F-ENTITY + RSK-F-PEP + RSK-F-MSB
                            + RSK-F-INDUSTRY + RSK-F-GEOGRAPHY
                            + RSK-F-PRODUCT + RSK-F-TENURE
                            + RSK-F-CASH + RSK-F-OTHER
           IF WS-TOTAL < 0
               MOVE 0 TO WS-TOTAL
           END-IF
           IF WS-TOTAL > 999
               MOVE 999 TO WS-TOTAL
           END-IF
      *    CONFIRMED OFAC MATCH OVERRIDES THE SCORE
           IF RSK-OFAC-STATUS = 'R'
               MOVE 999 TO WS-TOTAL
           END-IF
           MOVE WS-TOTAL TO RSK-RESULT-SCORE
           EVALUATE TRUE
               WHEN RSK-OFAC-STATUS = 'R'
               WHEN RSK-RESULT-SCORE >= WS-BAND-PROHIBITED
                   MOVE 'P' TO RSK-RESULT-RATING
                   MOVE 'Y' TO RSK-PROHIBITED-FLAG
                   MOVE 08  TO RSK-RETURN-CODE
               WHEN RSK-RESULT-SCORE >= WS-BAND-HIGH
                   MOVE 'H' TO RSK-RESULT-RATING
               WHEN RSK-RESULT-SCORE >= WS-BAND-MEDIUM
                   MOVE 'M' TO RSK-RESULT-RATING
               WHEN OTHER
                   MOVE 'L' TO RSK-RESULT-RATING
           END-EVALUATE.
