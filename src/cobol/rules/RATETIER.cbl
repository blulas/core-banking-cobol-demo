      *================================================================*
      * PROGRAM  : RATETIER                                            *
      * TYPE     : CALLED RULE MODULE (BATCH AND ONLINE)               *
      * PURPOSE  : SELECTS THE INTEREST RATE FOR A DEPOSIT ACCOUNT     *
      *            AND COMPUTES THE DISCLOSED ANNUAL PERCENTAGE YIELD. *
      *              1. PROMOTIONAL RATE WINS WHILE AS-OF DATE IS ON   *
      *                 OR BEFORE THE PROMO EXPIRY DATE                *
      *              2. OTHERWISE THE RATE TABLE TIER (RATETBL) BY     *
      *                 BALANCE, TERM OR INDEX                         *
      *              3. RELATIONSHIP / EMPLOYEE BONUS - NEVER ON CDS   *
      *                 OR PROMOTIONAL RATES                           *
      *              4. ALCO RATE CAP (NOT APPLIED TO CDS)             *
      *              5. APY PER REG DD APPENDIX A (365-DAY YEAR)       *
      * LINKAGE  : RTTPARM                                             *
      * CALLS    : GETCTL (FED FUNDS INDEX FOR CMM1)                   *
      * CALLED BY: INTACCR, CDMATUR, RATEINQ (CB20), ACCTOPEN          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 1994-10-03 RJT  APY DISCLOSURE (TRUTH IN SAVINGS ACT)          *
      * 1999-01-04 MKL  JUMBO MMKT TIER                                *
      * 2008-11-24 DPS  CD TERM NOT IN TABLE USES NEAREST SHORTER TERM *
      *                 (WAS: REJECT RC 12) - INC10442                 *
      * 2017-02-08 AKW  PROMO RATE PRECEDENCE                          *
      * 2019-08-19 AKW  RELATIONSHIP BONUS - CHG1655                   *
      * 2021-03-02 AKW  EMPLOYEE BONUS - CHG1904                       *
      * 2023-02-06 JQP  CMM1 INDEX PRICING WITH FLOOR - CHG2210        *
      * 2024-06-17 JQP  RATE CAP NOW ALSO APPLIES TO PROMO - INC22871  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATETIER.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RATETBL.
       COPY BANKCTL.

       01  WS-WORK-FIELDS.
           05  WS-TABLE-ID                 PIC X(04).
           05  WS-COMPOUND                 PIC X(01).
           05  WS-REL-BONUS-ELIG           PIC X(01).
           05  WS-EMP-BONUS-ELIG           PIC X(01).
           05  WS-TIME-DEPOSIT             PIC X(01).
               88  WS-IS-TIME-DEPOSIT                VALUE 'Y'.
           05  WS-PROMO-FLAG               PIC X(01).
               88  WS-PROMO-ACTIVE                   VALUE 'Y'.
           05  WS-FOUND-FLAG               PIC X(01).
               88  WS-TIER-FOUND                     VALUE 'Y'.
           05  WS-RATE                     PIC S9(03)V9(06).
           05  WS-BONUS                    PIC S9(03)V9(06).
           05  WS-INDEX-RATE               PIC S9(03)V9(06).
           05  WS-WARNING                  PIC X(01).

       01  WS-APY-FIELDS.
           05  WS-PERIODS                  PIC 9(03) COMP.
           05  WS-PERIOD-RATE              PIC 9(01)V9(15) COMP-3.
           05  WS-FACTOR                   PIC 9(03)V9(15) COMP-3.
           05  WS-APY                      PIC 9(03)V9(06).

       LINKAGE SECTION.
       COPY RTTPARM.

       PROCEDURE DIVISION USING RATETIER-PARMS.
       0000-MAIN.
           MOVE ZERO   TO RTT-RESULT-RATE RTT-RESULT-APY
                          RTT-RETURN-CODE
           MOVE SPACES TO RTT-TIER-ID
           MOVE 'N'    TO WS-PROMO-FLAG WS-WARNING
           MOVE ZERO   TO WS-RATE WS-BONUS

           PERFORM 1000-FIND-PRODUCT
           IF RTT-RETURN-CODE NOT = 00
               GOBACK
           END-IF

           PERFORM 2000-CHECK-PROMO
           IF NOT WS-PROMO-ACTIVE
               PERFORM 3000-TABLE-RATE
               IF RTT-RETURN-CODE NOT = 00
                   GOBACK
               END-IF
               PERFORM 4000-APPLY-BONUS
           END-IF
           PERFORM 5000-APPLY-CAP

           MOVE WS-RATE TO RTT-RESULT-RATE
           PERFORM 6000-COMPUTE-APY
           IF WS-WARNING = 'Y'
               MOVE 04 TO RTT-RETURN-CODE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
      * PRODUCT -> RATE TABLE. NONE = NOT INTEREST BEARING (RC 08).    *
      *----------------------------------------------------------------*
       1000-FIND-PRODUCT.
           SET RT-PM-IDX TO 1
           SEARCH RT-PMAP-ENTRY
               AT END
                   MOVE 'NONE' TO RTT-TIER-ID
                   MOVE 12     TO RTT-RETURN-CODE
               WHEN RT-PMAP-PRODUCT(RT-PM-IDX) = RTT-PRODUCT-CODE
                   MOVE RT-PMAP-TABLE-ID(RT-PM-IDX)  TO WS-TABLE-ID
                   MOVE RT-PMAP-COMPOUND(RT-PM-IDX)  TO WS-COMPOUND
                   MOVE RT-PMAP-REL-BONUS(RT-PM-IDX)
                     TO WS-REL-BONUS-ELIG
                   MOVE RT-PMAP-EMP-BONUS(RT-PM-IDX)
                     TO WS-EMP-BONUS-ELIG
                   MOVE RT-PMAP-TIME-DEP(RT-PM-IDX)  TO WS-TIME-DEPOSIT
           END-SEARCH
           IF RTT-RETURN-CODE = 00 AND WS-TABLE-ID = 'NONE'
               MOVE 'NONE' TO RTT-TIER-ID
               MOVE 08     TO RTT-RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
      * PROMOTIONAL RATE - VALID THROUGH AND INCLUDING EXPIRY DATE.    *
      * AN EXPIRED PROMO FALLS BACK TO THE TABLE WITH A WARNING SO     *
      * THE CALLER CAN CLEAR THE PROMO FIELDS. CDS HAVE NO PROMOS.     *
      *----------------------------------------------------------------*
       2000-CHECK-PROMO.
           IF RTT-PROMO-RATE > ZERO AND NOT WS-IS-TIME-DEPOSIT
               IF RTT-AS-OF-DATE <= RTT-PROMO-EXP-DATE
                   SET WS-PROMO-ACTIVE TO TRUE
                   MOVE RTT-PROMO-RATE TO WS-RATE
                   MOVE 'PRMO'         TO RTT-TIER-ID
               ELSE
                   MOVE 'Y' TO WS-WARNING
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * TABLE LOOKUP. TIERS ARE ASCENDING; THE LAST TIER WHOSE LOW     *
      * BOUND (OR TERM) IS <= THE ACCOUNT VALUE WINS. A BALANCE OR     *
      * TERM BELOW THE FIRST TIER TAKES THE FIRST TIER.                *
      *----------------------------------------------------------------*
       3000-TABLE-RATE.
           MOVE 'N' TO WS-FOUND-FLAG
           PERFORM VARYING RT-TR-IDX FROM 1 BY 1
                   UNTIL RT-TR-IDX > RT-TIER-COUNT
               IF RT-TIER-TABLE-ID(RT-TR-IDX) = WS-TABLE-ID
                   EVALUATE TRUE
                       WHEN RT-TIER-BY-INDEX(RT-TR-IDX)
                           PERFORM 3100-INDEX-RATE
                           SET WS-TIER-FOUND TO TRUE
                       WHEN RT-TIER-BY-BALANCE(RT-TR-IDX)
                           IF NOT WS-TIER-FOUND
                              OR RTT-BALANCE >=
                                 RT-TIER-LOW-BAL(RT-TR-IDX)
                               MOVE RT-TIER-RATE(RT-TR-IDX) TO WS-RATE
                               MOVE RT-TIER-ID(RT-TR-IDX)
                                 TO RTT-TIER-ID
                               SET WS-TIER-FOUND TO TRUE
                           END-IF
                       WHEN RT-TIER-BY-TERM(RT-TR-IDX)
                           IF NOT WS-TIER-FOUND
                              OR RTT-TERM-MONTHS >=
                                 RT-TIER-TERM(RT-TR-IDX)
                               MOVE RT-TIER-RATE(RT-TR-IDX) TO WS-RATE
                               MOVE RT-TIER-ID(RT-TR-IDX)
                                 TO RTT-TIER-ID
                               SET WS-TIER-FOUND TO TRUE
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF NOT WS-TIER-FOUND
               MOVE 12 TO RTT-RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
      * CMM1 - FED FUNDS EFFECTIVE MINUS SPREAD, NOT BELOW THE FLOOR.  *
      * (PRE-2023 BALANCE TIERS LEFT IN RATETBL AS TABLE CMB1)         *
      *----------------------------------------------------------------*
       3100-INDEX-RATE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           COMPUTE WS-INDEX-RATE = CTL-FED-FUNDS-RATE
                                 - RT-TIER-RATE(RT-TR-IDX)
           IF WS-INDEX-RATE < RT-INDEX-FLOOR-RATE
               MOVE RT-INDEX-FLOOR-RATE TO WS-INDEX-RATE
               MOVE 'Y' TO WS-WARNING
           END-IF
           MOVE WS-INDEX-RATE TO WS-RATE
           MOVE 'IDX ' TO RTT-TIER-ID.
      *    2023-02 JQP - BALANCE-TIER PRICING REPLACED (CHG2210)
      *    MOVE 'CMB1' TO WS-TABLE-ID
      *    PERFORM 3000-TABLE-RATE

      *----------------------------------------------------------------*
      * BONUSES. RELATIONSHIP BONUS ON HYSV/MMKT WHEN THE TOTAL        *
      * RELATIONSHIP BALANCE IS AT LEAST $250,000 (PRIVATE WEALTH      *
      * +0.15, OTHERS +0.10). EMPLOYEE BONUS +0.25 ON HYSV. THE TWO    *
      * BONUSES ARE ADDITIVE. NO BONUS ON TIME DEPOSITS.               *
      *----------------------------------------------------------------*
       4000-APPLY-BONUS.
           IF WS-IS-TIME-DEPOSIT
               EXIT PARAGRAPH
           END-IF
           IF WS-REL-BONUS-ELIG = 'Y'
              AND RTT-REL-BALANCE >= RT-REL-BONUS-MIN-BAL
               IF RTT-CUST-SEGMENT = 'PW'
                   ADD RT-PW-BONUS-RATE  TO WS-BONUS
               ELSE
                   ADD RT-REL-BONUS-RATE TO WS-BONUS
               END-IF
           END-IF
           IF WS-EMP-BONUS-ELIG = 'Y' AND RTT-EMPLOYEE-FLAG = 'Y'
               ADD RT-EMP-BONUS-RATE TO WS-BONUS
           END-IF
           ADD WS-BONUS TO WS-RATE.

      *----------------------------------------------------------------*
      * ALCO CAP - ALL NON-CD RATES INCLUDING PROMOTIONS (INC22871)    *
      *----------------------------------------------------------------*
       5000-APPLY-CAP.
           IF NOT WS-IS-TIME-DEPOSIT
              AND WS-RATE > RT-MAX-DEPOSIT-RATE
               MOVE RT-MAX-DEPOSIT-RATE TO WS-RATE
               MOVE 'Y' TO WS-WARNING
           END-IF.

      *----------------------------------------------------------------*
      * APY = 100 * ((1 + R/N) ** N - 1)                               *
      *   N = 365 DAILY COMPOUNDING, 12 MONTHLY, ELSE APY = RATE       *
      * DONE AS A LOOP - THE 1994 COMPILER HAD NO FLOATING POINT       *
      * EXPONENT WE TRUSTED FOR DISCLOSURES.                           *
      *----------------------------------------------------------------*
       6000-COMPUTE-APY.
           EVALUATE WS-COMPOUND
               WHEN 'D'
                   MOVE 365 TO WS-PERIODS
               WHEN 'M'
                   MOVE 12  TO WS-PERIODS
               WHEN OTHER
                   MOVE 0   TO WS-PERIODS
           END-EVALUATE
           IF WS-PERIODS = 0 OR WS-RATE = ZERO
               MOVE WS-RATE TO RTT-RESULT-APY
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-PERIOD-RATE ROUNDED =
                   WS-RATE / 100 / WS-PERIODS
           MOVE 1 TO WS-FACTOR
           PERFORM WS-PERIODS TIMES
               COMPUTE WS-FACTOR ROUNDED =
                       WS-FACTOR * (1 + WS-PERIOD-RATE)
           END-PERFORM
           COMPUTE WS-APY ROUNDED = (WS-FACTOR - 1) * 100
           MOVE WS-APY TO RTT-RESULT-APY.
