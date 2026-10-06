      *================================================================*
      * PROGRAM  : ODDECIDE                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : PAY-OR-RETURN DECISION FOR A DEBIT THAT EXCEEDS THE *
      *            AVAILABLE BALANCE. DETERMINES OD PROTECTION         *
      *            TRANSFER, OVERDRAFT ELIGIBILITY AND THE OD / NSF    *
      *            FEE, INCLUDING ALL FEE WAIVERS.                     *
      *            DECISION SEQUENCE:                                  *
      *              1 SUFFICIENT FUNDS              -> PAY            *
      *              2 BANK-ORIGINATED / POOLED      -> PAY (NO FEE)   *
      *              3 OD PROTECTION TRANSFER ($100 INCREMENTS)        *
      *              4 OVERDRAFT ELIGIBILITY RULES   -> PAY OR RETURN  *
      *              5 FEE AND FEE WAIVERS                             *
      * LINKAGE  : ODDPARM                                             *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-11 RJT  ORIGINAL - NSF FEE $15                         *
      * 1996-06-03 RJT  NSF FEE $20, OD LIMIT CHECK                    *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-04 BHS  OD PROTECTION TRANSFER FROM LINKED SAVINGS     *
      * 2006-01-09 BHS  FEES $29, MAX 5 PER DAY                        *
      * 2009-11-17 DPS  REG E OPT-IN FOR ATM / ONE-TIME DEBIT CARD     *
      *                 (REG-2010-E) EFFECTIVE 2010-07-01              *
      * 2010-07-01 DPS  SEPARATE OD / NSF FEE CODES, MAX 3 PER DAY     *
      * 2012-02-27 DPS  FEES $34; NO OD < 30 DAYS OPEN (CHG1102)       *
      * 2013-09-16 DPS  NO OD IF OVERDRAWN > 30 DAYS OR 6+ NSF IN 12M  *
      * 2015-10-02 DPS  CORE CONVERSION - PW SEGMENT FEE WAIVER        *
      * 2017-04-03 AKW  DE MINIMIS: NO FEE IF ITEM OR OD <= $5 CHG1894 *
      * 2018-06-11 AKW  EMPLOYEE 50 PCT FEE DISCOUNT (HR POLICY 4.12)  *
      * 2019-03-25 AKW  POOLED CORPORATE ACCOUNTS PAID VIA POOL        *
      * 2019-11-04 AKW  ZBA SUB-ACCOUNTS PAID - INC25120 RETURNED      *
      *                 ACME PAYROLL ON A ZERO BALANCE ACCOUNT         *
      * 2021-02-01 AKW  OUTGOING WIRES NEVER PAID INTO OD (INC22817)   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODDECIDE.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DATEPARM.

      *----------------------------------------------------------------*
      * OVERDRAFT POLICY PARAMETERS                                    *
      *----------------------------------------------------------------*
       01  WS-OD-POLICY.
           05  WS-OD-FEE                   PIC S9(05)V99 VALUE 34.00.
           05  WS-NSF-FEE                  PIC S9(05)V99 VALUE 34.00.
      *    2012-02 DPS WAS 29.00 / 29.00
           05  WS-XFER-FEE                 PIC S9(05)V99 VALUE 10.00.
           05  WS-XFER-INCREMENT           PIC S9(05)V99 VALUE 100.00.
           05  WS-MAX-FEES-PER-DAY         PIC 9(02)     VALUE 03.
           05  WS-DE-MINIMIS-ITEM          PIC S9(05)V99 VALUE 5.00.
           05  WS-DE-MINIMIS-OD            PIC S9(05)V99 VALUE 5.00.
           05  WS-MIN-ACCT-AGE-DAYS        PIC 9(03)     VALUE 030.
           05  WS-MAX-DAYS-OVERDRAWN       PIC 9(03)     VALUE 030.
           05  WS-MAX-NSF-12M              PIC 9(03)     VALUE 005.
           05  WS-EMPLOYEE-DISC-PCT        PIC 9(03)     VALUE 050.

       01  WS-WORK.
           05  WS-SHORTFALL                PIC S9(13)V99 COMP-3.
           05  WS-AVAIL-AFTER-XFER         PIC S9(13)V99 COMP-3.
           05  WS-RESULT-BAL               PIC S9(13)V99 COMP-3.
           05  WS-OD-AMOUNT                PIC S9(13)V99 COMP-3.
           05  WS-INCREMENTS               PIC S9(09)    COMP-3.
           05  WS-ACCT-AGE-DAYS            PIC S9(07).
           05  WS-ELIGIBLE                 PIC X(01).
           05  WS-FORCE-POST               PIC X(01).

       LINKAGE SECTION.
       COPY ODDPARM.

       PROCEDURE DIVISION USING ODDECIDE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE

      *    1. SUFFICIENT FUNDS
           IF ODD-AVAIL-BAL NOT < ODD-AMOUNT
               SET ODD-PAY TO TRUE
               MOVE 'FUND' TO ODD-REASON-CODE
               COMPUTE ODD-RESULT-AVAIL = ODD-AVAIL-BAL - ODD-AMOUNT
               MOVE 00 TO ODD-RETURN-CODE
               GOBACK
           END-IF

      *    2. BANK-ORIGINATED DEBITS AND POOLED CORPORATE ACCOUNTS
           PERFORM 2000-CHECK-FORCE-POST
           IF WS-FORCE-POST = 'Y'
               SET ODD-PAY TO TRUE
               COMPUTE ODD-RESULT-AVAIL = ODD-AVAIL-BAL - ODD-AMOUNT
               IF ODD-RESULT-AVAIL < 0
                   MOVE 'Y' TO ODD-PAID-INTO-OD
               END-IF
               MOVE 04 TO ODD-RETURN-CODE
               GOBACK
           END-IF

      *    3. OD PROTECTION TRANSFER
           MOVE ODD-AVAIL-BAL TO WS-AVAIL-AFTER-XFER
           IF ODD-HAS-PROTECTION AND ODD-PROTECT-AVAIL-BAL > 0
               PERFORM 3000-PROTECTION-TRANSFER
               IF WS-AVAIL-AFTER-XFER NOT < ODD-AMOUNT
                   SET ODD-PAY-WITH-XFER TO TRUE
                   MOVE 'XFER' TO ODD-REASON-CODE
                   COMPUTE ODD-RESULT-AVAIL = WS-AVAIL-AFTER-XFER
                                            - ODD-AMOUNT
                                            - ODD-XFER-FEE
                   MOVE 04 TO ODD-RETURN-CODE
                   GOBACK
               END-IF
           END-IF

      *    4. OVERDRAFT ELIGIBILITY
           PERFORM 4000-OD-ELIGIBILITY
           IF WS-ELIGIBLE = 'Y'
               IF ODD-XFER-AMOUNT > 0
                   SET ODD-PAY-WITH-XFER TO TRUE
               ELSE
                   SET ODD-PAY TO TRUE
               END-IF
               MOVE 'Y'    TO ODD-PAID-INTO-OD
               MOVE 'ODPD' TO ODD-REASON-CODE
               MOVE 'WFOD' TO ODD-FEE-CODE
               MOVE WS-OD-FEE TO ODD-FEE-AMOUNT
               COMPUTE WS-RESULT-BAL = WS-AVAIL-AFTER-XFER - ODD-AMOUNT
               MOVE 04 TO ODD-RETURN-CODE
           ELSE
      *        RETURNED ITEMS DO NOT TRIGGER A PROTECTION TRANSFER
               SET ODD-RETURN TO TRUE
               MOVE ZERO TO ODD-XFER-AMOUNT ODD-XFER-FEE
               MOVE ODD-AVAIL-BAL TO WS-RESULT-BAL
               IF ODD-REASON-CODE NOT = 'REGE'
                   MOVE 'WFNS' TO ODD-FEE-CODE
                   MOVE WS-NSF-FEE TO ODD-FEE-AMOUNT
               END-IF
               MOVE 08 TO ODD-RETURN-CODE
           END-IF

      *    5. FEE WAIVERS
           IF ODD-FEE-CODE NOT = SPACES
               PERFORM 5000-APPLY-FEE-WAIVERS
           END-IF
           COMPUTE ODD-RESULT-AVAIL = WS-RESULT-BAL
                                    - ODD-FEE-AMOUNT
                                    - ODD-XFER-FEE
           GOBACK.

       1000-INITIALISE.
           MOVE SPACES TO ODD-DECISION ODD-REASON-CODE ODD-FEE-CODE
                          ODD-FEE-WAIVE-CODE
           MOVE 'N'    TO ODD-PAID-INTO-OD
           MOVE ZERO   TO ODD-XFER-AMOUNT ODD-XFER-FEE ODD-FEE-AMOUNT
                          ODD-RESULT-AVAIL
           MOVE 'N'    TO WS-FORCE-POST WS-ELIGIBLE
           COMPUTE WS-SHORTFALL = ODD-AMOUNT - ODD-AVAIL-BAL.

      *----------------------------------------------------------------*
      * BANK-ORIGINATED DEBITS (FEES, PENALTIES, WITHHOLDING, ESCHEAT, *
      * ADJUSTMENTS, SWEEPS, MATURITY PAYOUTS) ARE ALWAYS POSTED AND   *
      * NEVER CARRY AN OD FEE. POOLED CORPORATE ACCOUNTS ARE COVERED   *
      * BY THE NOTIONAL POOL (POOLCALC) AND ARE ALWAYS PAID.           *
      *----------------------------------------------------------------*
       2000-CHECK-FORCE-POST.
           EVALUATE TRUE
               WHEN ODD-TRAN-CODE(1:2) = 'WF'
               WHEN ODD-TRAN-CODE = 'WPEN'
               WHEN ODD-TRAN-CODE = 'WBWH'
               WHEN ODD-TRAN-CODE = 'WESC'
               WHEN ODD-TRAN-CODE = 'WSWP'
               WHEN ODD-TRAN-CODE = 'WMAT'
               WHEN ODD-TRAN-CODE = 'ADJD'
                   MOVE 'Y'    TO WS-FORCE-POST
                   MOVE 'FRCD' TO ODD-REASON-CODE
               WHEN ODD-POOL-FLAG = 'Y'
                   MOVE 'Y'    TO WS-FORCE-POST
                   MOVE 'POOL' TO ODD-REASON-CODE
      *        ZBA DEBITS ARE FUNDED BY THE PARENT IN SWEEPRUN (STEP 10)
               WHEN ODD-POOL-FLAG = 'Z'
                   MOVE 'Y'    TO WS-FORCE-POST
                   MOVE 'ZBAF' TO ODD-REASON-CODE
               WHEN OTHER
                   MOVE 'N'    TO WS-FORCE-POST
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OD PROTECTION: TRANSFER THE SHORTFALL ROUNDED UP TO THE NEXT   *
      * $100. IF THE PROTECTING ACCOUNT CANNOT COVER THE ROUNDED       *
      * AMOUNT, TRANSFER ITS ENTIRE AVAILABLE BALANCE.                 *
      * TRANSFER FEE $10.00 - WAIVED FOR PRIVATE WEALTH.               *
      *----------------------------------------------------------------*
       3000-PROTECTION-TRANSFER.
           DIVIDE WS-SHORTFALL BY WS-XFER-INCREMENT
               GIVING WS-INCREMENTS
           IF WS-INCREMENTS * WS-XFER-INCREMENT < WS-SHORTFALL
               ADD 1 TO WS-INCREMENTS
           END-IF
           COMPUTE ODD-XFER-AMOUNT = WS-INCREMENTS * WS-XFER-INCREMENT
           IF ODD-XFER-AMOUNT > ODD-PROTECT-AVAIL-BAL
               MOVE ODD-PROTECT-AVAIL-BAL TO ODD-XFER-AMOUNT
           END-IF
           IF ODD-CUST-SEGMENT = 'PW'
               MOVE ZERO TO ODD-XFER-FEE
           ELSE
               MOVE WS-XFER-FEE TO ODD-XFER-FEE
           END-IF
           COMPUTE WS-AVAIL-AFTER-XFER = ODD-AVAIL-BAL
                                       + ODD-XFER-AMOUNT.

      *----------------------------------------------------------------*
      * OVERDRAFT ELIGIBILITY - FIRST FAILING RULE SETS THE REASON     *
      *----------------------------------------------------------------*
       4000-OD-ELIGIBILITY.
           PERFORM 4100-ACCOUNT-AGE
           COMPUTE WS-OD-AMOUNT = ODD-AMOUNT - WS-AVAIL-AFTER-XFER
           EVALUATE TRUE
               WHEN ODD-PROD-OD-ALLOWED NOT = 'Y'
                   MOVE 'NOOD' TO ODD-REASON-CODE
      *        REG E 1005.17 - ATM AND ONE-TIME DEBIT CARD ITEMS MAY
      *        ONLY BE PAID INTO OVERDRAFT IF THE CONSUMER OPTED IN.
      *        A DECLINED ITEM CARRIES NO FEE.
               WHEN (ODD-TRAN-CODE = 'WATM' OR ODD-TRAN-CODE = 'WPOS'
                     OR (ODD-TRAN-CODE = 'WCSH'
                         AND ODD-CHANNEL = 'ATM'))
                    AND ODD-OD-OPT-IN NOT = 'Y'
                   MOVE 'REGE' TO ODD-REASON-CODE
               WHEN ODD-TRAN-CODE = 'WWIR'
                   MOVE 'WIRE' TO ODD-REASON-CODE
               WHEN WS-ACCT-AGE-DAYS < WS-MIN-ACCT-AGE-DAYS
                   MOVE 'NEWA' TO ODD-REASON-CODE
               WHEN ODD-DAYS-OVERDRAWN > WS-MAX-DAYS-OVERDRAWN
                   MOVE 'ODAG' TO ODD-REASON-CODE
               WHEN ODD-NSF-COUNT-12M > WS-MAX-NSF-12M
                   MOVE 'NSFX' TO ODD-REASON-CODE
      *        CHECKS, ACH AND OTHER ITEMS ARE PAID AT BANK DISCRETION
      *        UP TO AND INCLUDING THE OVERDRAFT LIMIT
               WHEN WS-OD-AMOUNT > ODD-OD-LIMIT
                   MOVE 'OLIM' TO ODD-REASON-CODE
               WHEN OTHER
                   MOVE 'Y' TO WS-ELIGIBLE
           END-EVALUATE.

       4100-ACCOUNT-AGE.
           MOVE 99999 TO WS-ACCT-AGE-DAYS
           IF ODD-ACCT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ODD-ACCT-OPEN-DATE TO DTU-DATE-1
               MOVE ODD-BUSINESS-DATE  TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-ACCT-AGE-DAYS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * FEE WAIVERS - IN THIS ORDER                                    *
      *   PRIVATE WEALTH          NO FEE                      PWWV     *
      *   3 FEES ALREADY TODAY    NO FEE                      FCAP     *
      *   ITEM <= $5.00           NO FEE                      DMIN     *
      *   PAID AND OD <= $5.00    NO FEE                      DMOD     *
      *   EMPLOYEE                50 PCT OF FEE               EMPL     *
      * A FEE THAT IS CHARGED COUNTS TOWARD THE DAILY MAXIMUM.         *
      *----------------------------------------------------------------*
       5000-APPLY-FEE-WAIVERS.
           EVALUATE TRUE
               WHEN ODD-CUST-SEGMENT = 'PW'
                   MOVE 'PWWV' TO ODD-FEE-WAIVE-CODE
               WHEN ODD-FEES-TODAY NOT < WS-MAX-FEES-PER-DAY
                   MOVE 'FCAP' TO ODD-FEE-WAIVE-CODE
               WHEN ODD-AMOUNT NOT > WS-DE-MINIMIS-ITEM
                   MOVE 'DMIN' TO ODD-FEE-WAIVE-CODE
               WHEN ODD-INTO-OVERDRAFT
                    AND (0 - WS-RESULT-BAL) NOT > WS-DE-MINIMIS-OD
                   MOVE 'DMOD' TO ODD-FEE-WAIVE-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF ODD-FEE-WAIVE-CODE NOT = SPACES
               MOVE ZERO   TO ODD-FEE-AMOUNT
               MOVE SPACES TO ODD-FEE-CODE
           ELSE
               IF ODD-EMPLOYEE-FLAG = 'Y'
                   COMPUTE ODD-FEE-AMOUNT ROUNDED =
                       ODD-FEE-AMOUNT * (100 - WS-EMPLOYEE-DISC-PCT)
                                      / 100
                   MOVE 'EMPL' TO ODD-FEE-WAIVE-CODE
               END-IF
               ADD 1 TO ODD-FEES-TODAY
           END-IF.
