      *================================================================*
      * PROGRAM  : CRDECIDE                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CREDIT DECISION.                                    *
      *            CONSUMER (LPER) - STATE, IDENTITY, CREDIT SCORE,    *
      *              BANKRUPTCY, AMOUNT LIMITS, AMOUNT TO INCOME,      *
      *              DEBT-TO-INCOME (INCLUDING THE NEW PAYMENT) AND    *
      *              TIME IN EMPLOYMENT. COUNTEROFFER WHEN ONLY THE    *
      *              DTI TEST FAILS.                                   *
      *            BUSINESS - RISK GRADE, DSCR, CRE LOAN TO VALUE.     *
      *            RETURNS APPROVE / REFER / DECLINE AND UP TO FOUR    *
      *            PRINCIPAL REASONS IN PRIORITY ORDER (REG B ADVERSE  *
      *            ACTION NOTICE).                                     *
      * LINKAGE  : CRDPARM                                             *
      * CALLS    : AMORTCLC (NEW PAYMENT, COUNTEROFFER PRINCIPAL)      *
      * CALLED BY: LNORIG (CB42)                                       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2016-04-25 AKW  ORIGINAL - LPER LAUNCH                         *
      * 2016-11-14 AKW  STATE AVAILABILITY TABLE - LEGAL-2016-114      *
      * 2017-09-11 AKW  COUNTEROFFER AT 36% DTI - CHG2231              *
      * 2018-02-26 AKW  EXISTING CUSTOMER 720+ BYPASSES DTI REFER BAND *
      * 2019-02-04 JPL  MAX AMOUNT 35,000 -> 40,000 - CHG2689          *
      * 2019-06-03 JPL  BUSINESS RULES MOVED FROM CLS CREDIT MEMO PGM  *
      * 2021-08-30 JPL  KYC STATUS CHECK (CIP) - REG-2021-K04          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRDECIDE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * CONSUMER POLICY LIMITS                                         *
      *----------------------------------------------------------------*
       01  WS-CONSUMER-POLICY.
           05  POL-MIN-SCORE               PIC 9(03)    VALUE 660.
           05  POL-MIN-AMOUNT              PIC 9(07)V99 VALUE 3500.00.
           05  POL-MAX-AMOUNT              PIC 9(07)V99 VALUE 40000.00.
           05  POL-MAX-AMT-INCOME-PCT      PIC 9(03)V99 VALUE 50.00.
           05  POL-DTI-DECLINE-PCT         PIC 9(03)V99 VALUE 40.00.
           05  POL-DTI-REFER-PCT           PIC 9(03)V99 VALUE 36.00.
           05  POL-COUNTER-DTI-PCT         PIC 9(03)V99 VALUE 36.00.
           05  POL-MIN-EMPLOY-MONTHS       PIC 9(03)    VALUE 6.
           05  POL-EXISTING-BYPASS-SCORE   PIC 9(03)    VALUE 720.
      *    RATE USED FOR THE DTI PAYMENT WHEN NO PRICE IS SUPPLIED
           05  POL-DEFAULT-DTI-RATE        PIC 9(03)V9(06)
                                           VALUE 19.990000.
      *----------------------------------------------------------------*
      * STATES WHERE LPER IS NOT OFFERED (LEGAL-2016-114)              *
      *----------------------------------------------------------------*
       01  WS-EXCLUDED-STATE-VALUES.
           05  FILLER  PIC X(02) VALUE 'IA'.
           05  FILLER  PIC X(02) VALUE 'VT'.
           05  FILLER  PIC X(02) VALUE 'WV'.
           05  FILLER  PIC X(02) VALUE 'PR'.
       01  WS-EXCLUDED-STATES REDEFINES WS-EXCLUDED-STATE-VALUES.
           05  WS-EXCL-STATE   PIC X(02) OCCURS 4 TIMES
                               INDEXED BY EXS-IDX.
      *----------------------------------------------------------------*
      * BUSINESS POLICY                                                *
      *----------------------------------------------------------------*
       01  WS-BUSINESS-POLICY.
           05  POL-MAX-GRADE               PIC 9(02)    VALUE 07.
           05  POL-REFER-GRADE             PIC 9(02)    VALUE 06.
           05  POL-MIN-DSCR                PIC 9(02)V99 VALUE 1.25.
           05  POL-CRE-TARGET-DSCR         PIC 9(02)V99 VALUE 1.35.
           05  POL-CRE-MAX-LTV             PIC 9(03)V99 VALUE 75.00.
      *----------------------------------------------------------------*
      * REASON CODE TABLE                                              *
      *----------------------------------------------------------------*
       01  WS-REASON-TABLE-VALUES.
           05  FILLER PIC X(43)
               VALUE 'D01CREDIT SCORE BELOW PROGRAM MINIMUM      '.
           05  FILLER PIC X(43)
               VALUE 'D02BANKRUPTCY WITHIN LAST 7 YEARS          '.
           05  FILLER PIC X(43)
               VALUE 'D03AMOUNT OUTSIDE PRODUCT LIMITS           '.
           05  FILLER PIC X(43)
               VALUE 'D04AMOUNT REQUESTED EXCESSIVE FOR INCOME   '.
           05  FILLER PIC X(43)
               VALUE 'D05DEBT OBLIGATIONS TOO HIGH FOR INCOME    '.
           05  FILLER PIC X(43)
               VALUE 'D06PRODUCT NOT AVAILABLE IN YOUR STATE     '.
           05  FILLER PIC X(43)
               VALUE 'D07UNABLE TO VERIFY IDENTITY               '.
           05  FILLER PIC X(43)
               VALUE 'D08TERM REQUESTED NOT AVAILABLE            '.
           05  FILLER PIC X(43)
               VALUE 'D09INSUFFICIENT INCOME                     '.
           05  FILLER PIC X(43)
               VALUE 'R01DEBT TO INCOME REQUIRES REVIEW          '.
           05  FILLER PIC X(43)
               VALUE 'R02LENGTH OF EMPLOYMENT                    '.
           05  FILLER PIC X(43)
               VALUE 'R03IDENTITY VERIFICATION INCOMPLETE        '.
           05  FILLER PIC X(43)
               VALUE 'C01RISK GRADE OUTSIDE CREDIT POLICY        '.
           05  FILLER PIC X(43)
               VALUE 'C02DEBT SERVICE COVERAGE BELOW 1.25X       '.
           05  FILLER PIC X(43)
               VALUE 'C03LOAN TO VALUE ABOVE 75 PERCENT          '.
           05  FILLER PIC X(43)
               VALUE 'C04SENIOR CREDIT COMMITTEE APPROVAL REQD   '.
           05  FILLER PIC X(43)
               VALUE 'C05CRE DSCR BELOW 1.35X TARGET             '.
       01  WS-REASON-TABLE REDEFINES WS-REASON-TABLE-VALUES.
           05  WS-RSN-ENTRY OCCURS 17 TIMES INDEXED BY RSN-IDX.
               10  WS-RSN-CODE             PIC X(03).
               10  WS-RSN-TEXT             PIC X(40).
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-DECLINE-COUNT            PIC 9(02).
           05  WS-REFER-COUNT              PIC 9(02).
           05  WS-DECLINE-CODES.
               10  WS-DECLINE-CODE         PIC X(03) OCCURS 9 TIMES.
           05  WS-REFER-CODES.
               10  WS-REFER-CODE           PIC X(03) OCCURS 9 TIMES.
           05  WS-ADD-CODE                 PIC X(03).
           05  WS-I                        PIC 9(02).
           05  WS-MONTHLY-INCOME           PIC S9(11)V99 COMP-3.
           05  WS-MAX-PAYMENT              PIC S9(11)V99 COMP-3.
           05  WS-AMT-INCOME-PCT           PIC S9(05)V99 COMP-3.
           05  WS-COUNTER                  PIC S9(13)V99 COMP-3.
           05  WS-HUNDREDS                 PIC S9(11)    COMP-3.
           05  WS-DTI-ONLY-FLAG            PIC X(01).
           05  WS-DTI-RATE                 PIC 9(03)V9(06).
       COPY AMRTPARM.

       LINKAGE SECTION.
       COPY CRDPARM.

       PROCEDURE DIVISION USING CREDIT-DECISION-PARMS.
       0000-MAIN.
           INITIALIZE WS-WORK
           MOVE SPACES TO CRD-DECISION
           MOVE ZERO   TO CRD-NEW-PAYMENT CRD-DTI-PCT CRD-COUNTER-AMT
                          CRD-REASON-COUNT CRD-RETURN-CODE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE SPACES TO CRD-REASON-CODE(WS-I)
                              CRD-REASON-TEXT(WS-I)
           END-PERFORM

           EVALUATE TRUE
               WHEN CRD-CONSUMER
                   PERFORM 1000-CONSUMER-RULES
               WHEN CRD-BUSINESS
                   PERFORM 3000-BUSINESS-RULES
               WHEN OTHER
                   MOVE 12 TO CRD-RETURN-CODE
                   GOBACK
           END-EVALUATE

           PERFORM 8000-SET-DECISION
           GOBACK.

      *================================================================*
      * CONSUMER RULES - EVALUATED IN ADVERSE ACTION PRIORITY ORDER    *
      *================================================================*
       1000-CONSUMER-RULES.
           PERFORM 1100-STATE-AVAILABILITY
           PERFORM 1200-IDENTITY
           PERFORM 1300-CREDIT-SCORE
           PERFORM 1400-BANKRUPTCY
           PERFORM 1500-AMOUNT-AND-TERM
           PERFORM 1600-AMOUNT-TO-INCOME
           PERFORM 1700-DEBT-TO-INCOME
           PERFORM 1800-EMPLOYMENT
           PERFORM 1900-COUNTEROFFER.

       1100-STATE-AVAILABILITY.
           SET EXS-IDX TO 1
           SEARCH WS-EXCL-STATE
               AT END
                   CONTINUE
               WHEN WS-EXCL-STATE(EXS-IDX) = CRD-STATE
                   MOVE 'D06' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
           END-SEARCH.

      *    CIP - KYC MUST BE VERIFIED BEFORE FUNDING (REG-2021-K04)
       1200-IDENTITY.
           EVALUATE CRD-KYC-STATUS
               WHEN 'V'
                   CONTINUE
               WHEN 'F'
                   MOVE 'D07' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
               WHEN 'P'
               WHEN 'E'
               WHEN 'R'
                   MOVE 'R03' TO WS-ADD-CODE
                   PERFORM 8200-ADD-REFER
               WHEN OTHER
                   MOVE 'R03' TO WS-ADD-CODE
                   PERFORM 8200-ADD-REFER
           END-EVALUATE.

       1300-CREDIT-SCORE.
           IF CRD-CREDIT-SCORE < POL-MIN-SCORE
               MOVE 'D01' TO WS-ADD-CODE
               PERFORM 8100-ADD-DECLINE
           END-IF.

       1400-BANKRUPTCY.
           IF CRD-BANKRUPTCY-7YR = 'Y'
               MOVE 'D02' TO WS-ADD-CODE
               PERFORM 8100-ADD-DECLINE
           END-IF.

       1500-AMOUNT-AND-TERM.
           IF CRD-REQUESTED-AMT < POL-MIN-AMOUNT
              OR CRD-REQUESTED-AMT > POL-MAX-AMOUNT
               MOVE 'D03' TO WS-ADD-CODE
               PERFORM 8100-ADD-DECLINE
           END-IF
           EVALUATE CRD-TERM-MONTHS
               WHEN 12
               WHEN 24
               WHEN 36
               WHEN 48
               WHEN 60
                   CONTINUE
               WHEN OTHER
                   MOVE 'D08' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
           END-EVALUATE.

       1600-AMOUNT-TO-INCOME.
           IF CRD-ANNUAL-INCOME NOT > ZERO
               MOVE 'D09' TO WS-ADD-CODE
               PERFORM 8100-ADD-DECLINE
           ELSE
               COMPUTE WS-AMT-INCOME-PCT ROUNDED =
                   CRD-REQUESTED-AMT / CRD-ANNUAL-INCOME * 100
               IF WS-AMT-INCOME-PCT > POL-MAX-AMT-INCOME-PCT
                   MOVE 'D04' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * DTI = (EXISTING MONTHLY DEBT + NEW PAYMENT) / MONTHLY INCOME   *
      *   > 40.00%            DECLINE                                  *
      *   > 36.00% - 40.00%   REFER (EXISTING CUSTOMER 720+ BYPASSES)  *
      *----------------------------------------------------------------*
       1700-DEBT-TO-INCOME.
           IF CRD-ANNUAL-INCOME NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-MONTHLY-INCOME ROUNDED = CRD-ANNUAL-INCOME / 12
           IF CRD-PROPOSED-RATE > ZERO
               MOVE CRD-PROPOSED-RATE TO WS-DTI-RATE
           ELSE
               MOVE POL-DEFAULT-DTI-RATE TO WS-DTI-RATE
           END-IF
           IF CRD-REQUESTED-AMT > ZERO AND CRD-TERM-MONTHS > ZERO
               INITIALIZE AMORT-PARMS
               MOVE 'PMT '             TO AMT-FUNCTION
               MOVE CRD-REQUESTED-AMT  TO AMT-PRINCIPAL
               MOVE WS-DTI-RATE        TO AMT-ANNUAL-RATE
               MOVE '3'                TO AMT-ACCRUAL-BASIS
               MOVE 'M'                TO AMT-PAYMENT-FREQ
               MOVE CRD-TERM-MONTHS    TO AMT-TERM-MONTHS
               CALL 'AMORTCLC' USING AMORT-PARMS
               IF AMT-OK
                   MOVE AMT-PAYMENT-AMT TO CRD-NEW-PAYMENT
               END-IF
           END-IF
           COMPUTE CRD-DTI-PCT ROUNDED =
               (CRD-MONTHLY-DEBT + CRD-NEW-PAYMENT)
               / WS-MONTHLY-INCOME * 100
               ON SIZE ERROR MOVE 999.99 TO CRD-DTI-PCT
           END-COMPUTE
           EVALUATE TRUE
               WHEN CRD-DTI-PCT > POL-DTI-DECLINE-PCT
                   MOVE 'D05' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
               WHEN CRD-DTI-PCT > POL-DTI-REFER-PCT
                   IF CRD-EXISTING-CUST = 'Y'
                      AND CRD-CREDIT-SCORE NOT <
                          POL-EXISTING-BYPASS-SCORE
                       CONTINUE
                   ELSE
                       MOVE 'R01' TO WS-ADD-CODE
                       PERFORM 8200-ADD-REFER
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       1800-EMPLOYMENT.
           IF CRD-EMPLOYMENT-MONTHS < POL-MIN-EMPLOY-MONTHS
               MOVE 'R02' TO WS-ADD-CODE
               PERFORM 8200-ADD-REFER
           END-IF.

      *----------------------------------------------------------------*
      * COUNTEROFFER - ONLY WHEN DTI IS THE SOLE PROBLEM. LARGEST      *
      * AMOUNT WHOSE PAYMENT BRINGS DTI TO 36%, ROUNDED DOWN TO $100,  *
      * NOT BELOW THE $3,500 PRODUCT MINIMUM (CHG2231)                 *
      *----------------------------------------------------------------*
       1900-COUNTEROFFER.
           MOVE 'Y' TO WS-DTI-ONLY-FLAG
           IF WS-DECLINE-COUNT = 0 AND WS-REFER-COUNT = 0
               MOVE 'N' TO WS-DTI-ONLY-FLAG
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-DECLINE-COUNT
               IF WS-DECLINE-CODE(WS-I) NOT = 'D05'
                   MOVE 'N' TO WS-DTI-ONLY-FLAG
               END-IF
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-REFER-COUNT
               IF WS-REFER-CODE(WS-I) NOT = 'R01'
                   MOVE 'N' TO WS-DTI-ONLY-FLAG
               END-IF
           END-PERFORM
           IF WS-DTI-ONLY-FLAG NOT = 'Y'
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-MAX-PAYMENT =
               WS-MONTHLY-INCOME * POL-COUNTER-DTI-PCT / 100
               - CRD-MONTHLY-DEBT
           IF WS-MAX-PAYMENT NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           INITIALIZE AMORT-PARMS
           MOVE 'MAXP'             TO AMT-FUNCTION
           MOVE WS-MAX-PAYMENT     TO AMT-PAYMENT-AMT
           MOVE WS-DTI-RATE        TO AMT-ANNUAL-RATE
           MOVE '3'                TO AMT-ACCRUAL-BASIS
           MOVE 'M'                TO AMT-PAYMENT-FREQ
           MOVE CRD-TERM-MONTHS    TO AMT-TERM-MONTHS
           CALL 'AMORTCLC' USING AMORT-PARMS
           IF NOT AMT-OK
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-HUNDREDS = AMT-MAX-PRINCIPAL / 100
           COMPUTE WS-COUNTER  = WS-HUNDREDS * 100
           IF WS-COUNTER > POL-MAX-AMOUNT
               MOVE POL-MAX-AMOUNT TO WS-COUNTER
           END-IF
           IF WS-COUNTER NOT < POL-MIN-AMOUNT
              AND WS-COUNTER < CRD-REQUESTED-AMT
               MOVE WS-COUNTER TO CRD-COUNTER-AMT
           END-IF.

      *================================================================*
      * BUSINESS RULES                                                 *
      *================================================================*
       3000-BUSINESS-RULES.
           EVALUATE TRUE
               WHEN CRD-RISK-GRADE > POL-MAX-GRADE
               WHEN CRD-RISK-GRADE = ZERO
                   MOVE 'C01' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
               WHEN CRD-RISK-GRADE NOT < POL-REFER-GRADE
                   MOVE 'C04' TO WS-ADD-CODE
                   PERFORM 8200-ADD-REFER
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CRD-DSCR < POL-MIN-DSCR
               MOVE 'C02' TO WS-ADD-CODE
               PERFORM 8100-ADD-DECLINE
           END-IF
           IF CRD-PRODUCT-CODE = 'LCRE'
               IF CRD-LTV-PCT > POL-CRE-MAX-LTV
                   MOVE 'C03' TO WS-ADD-CODE
                   PERFORM 8100-ADD-DECLINE
               END-IF
               IF CRD-DSCR NOT < POL-MIN-DSCR
                  AND CRD-DSCR < POL-CRE-TARGET-DSCR
                   MOVE 'C05' TO WS-ADD-CODE
                   PERFORM 8200-ADD-REFER
               END-IF
           END-IF.

      *================================================================*
      * DECISION AND REASONS                                           *
      *================================================================*
       8000-SET-DECISION.
           EVALUATE TRUE
               WHEN WS-DECLINE-COUNT > 0
                   SET CRD-DECLINED TO TRUE
                   MOVE 08 TO CRD-RETURN-CODE
                   PERFORM VARYING WS-I FROM 1 BY 1
                           UNTIL WS-I > WS-DECLINE-COUNT
                              OR CRD-REASON-COUNT = 4
                       MOVE WS-DECLINE-CODE(WS-I) TO WS-ADD-CODE
                       PERFORM 8300-ADD-REASON-OUT
                   END-PERFORM
               WHEN WS-REFER-COUNT > 0
                   SET CRD-REFERRED TO TRUE
                   MOVE 04 TO CRD-RETURN-CODE
                   PERFORM VARYING WS-I FROM 1 BY 1
                           UNTIL WS-I > WS-REFER-COUNT
                              OR CRD-REASON-COUNT = 4
                       MOVE WS-REFER-CODE(WS-I) TO WS-ADD-CODE
                       PERFORM 8300-ADD-REASON-OUT
                   END-PERFORM
               WHEN OTHER
                   SET CRD-APPROVED TO TRUE
                   MOVE 00 TO CRD-RETURN-CODE
           END-EVALUATE.

       8100-ADD-DECLINE.
           IF WS-DECLINE-COUNT < 9
               ADD 1 TO WS-DECLINE-COUNT
               MOVE WS-ADD-CODE TO WS-DECLINE-CODE(WS-DECLINE-COUNT)
           END-IF.

       8200-ADD-REFER.
           IF WS-REFER-COUNT < 9
               ADD 1 TO WS-REFER-COUNT
               MOVE WS-ADD-CODE TO WS-REFER-CODE(WS-REFER-COUNT)
           END-IF.

       8300-ADD-REASON-OUT.
           ADD 1 TO CRD-REASON-COUNT
           MOVE WS-ADD-CODE TO CRD-REASON-CODE(CRD-REASON-COUNT)
           SET RSN-IDX TO 1
           SEARCH WS-RSN-ENTRY
               AT END
                   MOVE 'SEE CREDIT FILE' TO
                        CRD-REASON-TEXT(CRD-REASON-COUNT)
               WHEN WS-RSN-CODE(RSN-IDX) = WS-ADD-CODE
                   MOVE WS-RSN-TEXT(RSN-IDX) TO
                        CRD-REASON-TEXT(CRD-REASON-COUNT)
           END-SEARCH.
