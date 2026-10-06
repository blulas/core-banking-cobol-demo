      *================================================================*
      * PROGRAM  : LNPRICE                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOAN RATE PRICING.                                  *
      *            LPER - FIXED RATE FROM CREDIT SCORE GRID, PLUS TERM *
      *                   ADD-ON, LESS AUTOPAY / EMPLOYEE /            *
      *                   RELATIONSHIP DISCOUNTS, WITHIN 5.99%-24.99%. *
      *            LNTM / LLOC / LCRE - INDEX (PRIME OR SOFR) PLUS     *
      *                   MARGIN BY RISK GRADE, LESS RELATIONSHIP      *
      *                   DISCOUNT, FLOOR 4.00% / CAP 18.00% UNLESS    *
      *                   THE NOTE CARRIES ITS OWN FLOOR / CAP.        *
      * LINKAGE  : LNPRPARM                                            *
      * CALLED BY: LNORIG (CB42), LDLOAN, LOAN BOOKING                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-15 RJT  ORIGINAL - COMMERCIAL PRIME GRID               *
      * 1999-03-08 MKL  RATE FLOOR 4.00% ON ALL VARIABLE NOTES         *
      * 2004-10-19 BHS  RATE CAP 18.00% (STATE USURY REVIEW)           *
      * 2010-11-01 DPS  RELATIONSHIP DISCOUNT 0.25% AT $1MM DEPOSITS   *
      * 2016-04-25 AKW  LPER CONSUMER GRID - CREDIT SCORE BANDS        *
      * 2017-03-20 AKW  AUTOPAY DISCOUNT 0.25% - CHG2104               *
      * 2018-07-16 AKW  EMPLOYEE DISCOUNT 0.50% - HR-2018-07           *
      * 2022-01-10 AKW  SOFR MARGIN GRID (LIBOR CESSATION) REG-2022-L01*
      * 2023-05-08 JPL  SCORE 660-679 BAND REPRICED 18.99 -> 19.99     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNPRICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * LPER CREDIT SCORE GRID - LOW SCORE, RATE  (SCORE < 660 DECLINE)*
      *----------------------------------------------------------------*
       01  LPER-GRID-VALUES.
           05  FILLER  PIC X(12) VALUE '760007990000'.
           05  FILLER  PIC X(12) VALUE '740009490000'.
           05  FILLER  PIC X(12) VALUE '720010990000'.
           05  FILLER  PIC X(12) VALUE '700012990000'.
           05  FILLER  PIC X(12) VALUE '680015990000'.
           05  FILLER  PIC X(12) VALUE '660019990000'.
       01  LPER-GRID REDEFINES LPER-GRID-VALUES.
           05  LPG-ENTRY OCCURS 6 TIMES INDEXED BY LPG-IDX.
               10  LPG-LOW-SCORE           PIC 9(03).
               10  LPG-RATE                PIC 9(03)V9(06).
       01  LPER-MIN-SCORE                  PIC 9(03) VALUE 660.
       01  LPER-MIN-RATE                   PIC 9(03)V9(06)
                                           VALUE 5.990000.
       01  LPER-MAX-RATE                   PIC 9(03)V9(06)
                                           VALUE 24.990000.
      *----------------------------------------------------------------*
      * COMMERCIAL MARGIN GRID BY RISK GRADE - SOFR AND PRIME          *
      * GRADE 8 AND WORSE (SPECIAL MENTION / SUBSTANDARD) - DECLINE    *
      *----------------------------------------------------------------*
       01  COMM-GRID-VALUES.
      *        GRADE  SOFR MARGIN  PRIME MARGIN
           05  FILLER  PIC X(20) VALUE '01001750000000000000'.
           05  FILLER  PIC X(20) VALUE '02001750000000000000'.
           05  FILLER  PIC X(20) VALUE '03001750000000000000'.
           05  FILLER  PIC X(20) VALUE '04002500000000500000'.
           05  FILLER  PIC X(20) VALUE '05002500000000500000'.
           05  FILLER  PIC X(20) VALUE '06003250000001250000'.
           05  FILLER  PIC X(20) VALUE '07004250000002250000'.
       01  COMM-GRID REDEFINES COMM-GRID-VALUES.
           05  CMG-ENTRY OCCURS 7 TIMES INDEXED BY CMG-IDX.
               10  CMG-GRADE               PIC 9(02).
               10  CMG-SOFR-MARGIN         PIC 9(03)V9(06).
               10  CMG-PRIME-MARGIN        PIC 9(03)V9(06).
       01  COMM-MAX-GRADE                  PIC 9(02) VALUE 07.
       01  COMM-DEFAULT-FLOOR              PIC 9(03)V9(06)
                                           VALUE 4.000000.
       01  COMM-DEFAULT-CAP                PIC 9(03)V9(06)
                                           VALUE 18.000000.
      *----------------------------------------------------------------*
       01  WS-ADJUSTMENTS.
           05  WS-TERM-ADDON-48            PIC 9V99 VALUE 0.50.
           05  WS-TERM-ADDON-60            PIC 9V99 VALUE 1.00.
           05  WS-AUTOPAY-DISC             PIC 9V99 VALUE 0.25.
           05  WS-EMPLOYEE-DISC            PIC 9V99 VALUE 0.50.
           05  WS-REL-DISC                 PIC 9V99 VALUE 0.25.
           05  WS-REL-DISC-THRESHOLD       PIC 9(09)V99
                                           VALUE 1000000.00.
       01  WS-WORK-RATE                    PIC S9(03)V9(06).

       LINKAGE SECTION.
       COPY LNPRPARM.

       PROCEDURE DIVISION USING LNPRICE-PARMS.
       0000-MAIN.
           MOVE 00    TO LNP-RETURN-CODE
           MOVE ZERO  TO LNP-BASE-RATE LNP-MARGIN-RATE LNP-TOTAL-ADJ
                         LNP-FLOOR-RATE LNP-CAP-RATE LNP-FINAL-RATE
           MOVE 'N'   TO LNP-FLOOR-APPLIED LNP-CAP-APPLIED
           MOVE SPACES TO LNP-DECLINE-REASON LNP-RATE-TYPE

           EVALUATE LNP-PRODUCT-CODE
               WHEN 'LPER'
                   PERFORM 1000-PRICE-CONSUMER
               WHEN 'LNTM'
               WHEN 'LLOC'
               WHEN 'LCRE'
                   PERFORM 2000-PRICE-COMMERCIAL
               WHEN OTHER
                   MOVE 12 TO LNP-RETURN-CODE
                   MOVE 'P99' TO LNP-DECLINE-REASON
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
      * CONSUMER PERSONAL LOAN - FIXED RATE                            *
      *----------------------------------------------------------------*
       1000-PRICE-CONSUMER.
           MOVE 'F' TO LNP-RATE-TYPE
           IF LNP-CREDIT-SCORE < LPER-MIN-SCORE
               MOVE 08 TO LNP-RETURN-CODE
               MOVE 'P01' TO LNP-DECLINE-REASON
               EXIT PARAGRAPH
           END-IF
           IF LNP-TERM-MONTHS < 12 OR LNP-TERM-MONTHS > 60
               MOVE 08 TO LNP-RETURN-CODE
               MOVE 'P02' TO LNP-DECLINE-REASON
               EXIT PARAGRAPH
           END-IF

           SET LPG-IDX TO 1
           SEARCH LPG-ENTRY
               AT END
                   MOVE 08 TO LNP-RETURN-CODE
                   MOVE 'P01' TO LNP-DECLINE-REASON
                   EXIT PARAGRAPH
               WHEN LNP-CREDIT-SCORE NOT < LPG-LOW-SCORE(LPG-IDX)
                   MOVE LPG-RATE(LPG-IDX) TO LNP-BASE-RATE
           END-SEARCH

      *    TERM ADD-ON
           EVALUATE TRUE
               WHEN LNP-TERM-MONTHS > 48
                   ADD WS-TERM-ADDON-60 TO LNP-TOTAL-ADJ
               WHEN LNP-TERM-MONTHS > 36
                   ADD WS-TERM-ADDON-48 TO LNP-TOTAL-ADJ
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF LNP-AUTOPAY-FLAG = 'Y'
               SUBTRACT WS-AUTOPAY-DISC FROM LNP-TOTAL-ADJ
           END-IF
           IF LNP-EMPLOYEE-FLAG = 'Y'
               SUBTRACT WS-EMPLOYEE-DISC FROM LNP-TOTAL-ADJ
           END-IF
           IF LNP-REL-DEPOSIT-BAL NOT < WS-REL-DISC-THRESHOLD
               SUBTRACT WS-REL-DISC FROM LNP-TOTAL-ADJ
           END-IF

           COMPUTE WS-WORK-RATE = LNP-BASE-RATE + LNP-TOTAL-ADJ
           MOVE LPER-MIN-RATE TO LNP-FLOOR-RATE
           MOVE LPER-MAX-RATE TO LNP-CAP-RATE
           PERFORM 8000-APPLY-FLOOR-CAP.

      *----------------------------------------------------------------*
      * COMMERCIAL - INDEX + MARGIN BY GRADE                           *
      *----------------------------------------------------------------*
       2000-PRICE-COMMERCIAL.
           IF LNP-PRODUCT-CODE = 'LCRE'
               MOVE 'F' TO LNP-RATE-TYPE
           ELSE
               MOVE 'V' TO LNP-RATE-TYPE
           END-IF
           IF LNP-RISK-GRADE = ZERO
               MOVE 12 TO LNP-RETURN-CODE
               MOVE 'P98' TO LNP-DECLINE-REASON
               EXIT PARAGRAPH
           END-IF
           IF LNP-RISK-GRADE > COMM-MAX-GRADE
               MOVE 08 TO LNP-RETURN-CODE
               MOVE 'P03' TO LNP-DECLINE-REASON
               EXIT PARAGRAPH
           END-IF

           MOVE LNP-INDEX-RATE TO LNP-BASE-RATE
           SET CMG-IDX TO 1
           SEARCH CMG-ENTRY
               AT END
                   MOVE 12 TO LNP-RETURN-CODE
                   MOVE 'P98' TO LNP-DECLINE-REASON
                   EXIT PARAGRAPH
               WHEN CMG-GRADE(CMG-IDX) = LNP-RISK-GRADE
                   IF LNP-RATE-INDEX = 'PRIME'
                       MOVE CMG-PRIME-MARGIN(CMG-IDX)
                         TO LNP-MARGIN-RATE
                   ELSE
                       MOVE CMG-SOFR-MARGIN(CMG-IDX)
                         TO LNP-MARGIN-RATE
                   END-IF
           END-SEARCH

           IF LNP-REL-DEPOSIT-BAL NOT < WS-REL-DISC-THRESHOLD
               SUBTRACT WS-REL-DISC FROM LNP-TOTAL-ADJ
           END-IF

           COMPUTE WS-WORK-RATE =
               LNP-BASE-RATE + LNP-MARGIN-RATE + LNP-TOTAL-ADJ
           IF LNP-FLOOR-OVERRIDE > ZERO
               MOVE LNP-FLOOR-OVERRIDE TO LNP-FLOOR-RATE
           ELSE
               MOVE COMM-DEFAULT-FLOOR TO LNP-FLOOR-RATE
           END-IF
           IF LNP-CAP-OVERRIDE > ZERO
               MOVE LNP-CAP-OVERRIDE TO LNP-CAP-RATE
           ELSE
               MOVE COMM-DEFAULT-CAP TO LNP-CAP-RATE
           END-IF
           PERFORM 8000-APPLY-FLOOR-CAP.

      *----------------------------------------------------------------*
       8000-APPLY-FLOOR-CAP.
           EVALUATE TRUE
               WHEN WS-WORK-RATE < LNP-FLOOR-RATE
                   MOVE LNP-FLOOR-RATE TO LNP-FINAL-RATE
                   MOVE 'Y' TO LNP-FLOOR-APPLIED
               WHEN WS-WORK-RATE > LNP-CAP-RATE
                   MOVE LNP-CAP-RATE TO LNP-FINAL-RATE
                   MOVE 'Y' TO LNP-CAP-APPLIED
               WHEN OTHER
                   MOVE WS-WORK-RATE TO LNP-FINAL-RATE
           END-EVALUATE.
