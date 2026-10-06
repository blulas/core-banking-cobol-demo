      *================================================================*
      * PROGRAM  : ECRCALC                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : EARNINGS CREDIT ALLOWANCE (ECA) FOR COMMERCIAL      *
      *            ACCOUNT ANALYSIS.                                   *
      *              ECR  = BASE ECR (BANKCTL) + BALANCE TIER ADJ      *
      *                     (NOT TO EXCEED FED FUNDS)                  *
      *              INVESTABLE = AVG COLLECTED x (1 - RESERVE REQ)    *
      *              CREDIT = INVESTABLE x ECR x DAYS / 365            *
      *            BALANCE TIERS (AVERAGE COLLECTED BALANCE)           *
      *              UNDER  1,000,000.00               BASE            *
      *              1,000,000.00 - 10,000,000.00      BASE + 0.25%    *
      *              OVER  10,000,000.00               BASE + 0.50%    *
      *            MSB CUSTOMERS EARN NO CREDIT (BSA RISK POLICY).     *
      * CALLED BY: ANLYSFEE                                            *
      * LINKAGE  : ECRPARM                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-06-01 RJT  ORIGINAL - COMMERCIAL ACCOUNT ANALYSIS         *
      * 1998-11-02 MKL  Y2K REVIEW - NO DATE FIELDS                    *
      * 2003-04-07 BHS  CHG0877 ECR CAPPED AT FED FUNDS TARGET         *
      * 2008-12-17 DPS  INC10955 FED FUNDS 0-0.25% - CAP SUSPENDED     *
      *                 WHEN FED FUNDS IS ZERO                         *
      * 2011-04-12 DPS  CHG1544 THIRD BALANCE TIER OVER $10MM          *
      * 2019-10-07 AKW  CHG3310 NO EARNINGS CREDIT FOR MSB CUSTOMERS   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECRCALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ECR-CONSTANTS.
      *    FALLBACK WHEN BANKCTL RATE NOT SET
           05  WS-DEFAULT-BASE-ECR         PIC 9(03)V9(06)
                                           VALUE 2.500000.
      *    RESERVE REQUIREMENT ON TRANSACTION ACCOUNTS (REG D)
           05  WS-RESERVE-REQ-PCT          PIC 9(03)V9(06)
                                           VALUE 10.000000.
           05  WS-DAY-BASIS                PIC 9(03) VALUE 365.
       01  WS-ECR-TIER-TABLE.
           05  WS-TIER-1-CEILING           PIC S9(13)V99 COMP-3
                                           VALUE 1000000.00.
           05  WS-TIER-2-CEILING           PIC S9(13)V99 COMP-3
                                           VALUE 10000000.00.
           05  WS-TIER-1-ADJ               PIC 9(03)V9(06)
                                           VALUE 0.000000.
           05  WS-TIER-2-ADJ               PIC 9(03)V9(06)
                                           VALUE 0.250000.
           05  WS-TIER-3-ADJ               PIC 9(03)V9(06)
                                           VALUE 0.500000.
       01  WS-WORK.
           05  WS-BASE-RATE                PIC 9(03)V9(06).
           05  WS-CREDIT                   PIC S9(11)V9(06) COMP-3.

       LINKAGE SECTION.
       COPY ECRPARM.

       PROCEDURE DIVISION USING ECR-CALC-PARMS.
       0000-MAIN.
           MOVE ZERO   TO ECR-TIER-ADJ ECR-RATE ECR-INVESTABLE-BAL
                          ECR-EARNINGS-CREDIT ECR-RETURN-CODE
           MOVE SPACES TO ECR-REASON-CODE ECR-TIER-CODE
           MOVE WS-RESERVE-REQ-PCT TO ECR-RESERVE-PCT
           MOVE WS-DAY-BASIS       TO ECR-DAY-BASIS

           IF ECR-DAYS-IN-PERIOD = 0 OR ECR-DAYS-IN-PERIOD > 31
               MOVE 12     TO ECR-RETURN-CODE
               MOVE 'EDAY' TO ECR-REASON-CODE
               GOBACK
           END-IF

           PERFORM 1000-DETERMINE-RATE
           PERFORM 2000-COMPUTE-CREDIT
           GOBACK.

      *----------------------------------------------------------------*
      * EARNINGS CREDIT RATE                                           *
      *----------------------------------------------------------------*
       1000-DETERMINE-RATE.
           IF ECR-BASE-RATE > 0
               MOVE ECR-BASE-RATE       TO WS-BASE-RATE
           ELSE
               MOVE WS-DEFAULT-BASE-ECR TO WS-BASE-RATE
           END-IF

           EVALUATE TRUE
               WHEN ECR-AVG-COLLECTED-BAL < WS-TIER-1-CEILING
                   MOVE '1'           TO ECR-TIER-CODE
                   MOVE WS-TIER-1-ADJ TO ECR-TIER-ADJ
               WHEN ECR-AVG-COLLECTED-BAL <= WS-TIER-2-CEILING
                   MOVE '2'           TO ECR-TIER-CODE
                   MOVE WS-TIER-2-ADJ TO ECR-TIER-ADJ
               WHEN OTHER
                   MOVE '3'           TO ECR-TIER-CODE
                   MOVE WS-TIER-3-ADJ TO ECR-TIER-ADJ
           END-EVALUATE
           COMPUTE ECR-RATE = WS-BASE-RATE + ECR-TIER-ADJ

      *    CHG0877 - NEVER PAY MORE THAN FED FUNDS. INC10955 - THE CAP
      *    IS NOT APPLIED WHEN FED FUNDS IS ZERO (2008-2015 ZIRP)
           IF ECR-FED-FUNDS-RATE > 0
              AND ECR-RATE > ECR-FED-FUNDS-RATE
               MOVE ECR-FED-FUNDS-RATE TO ECR-RATE
               MOVE 'FCAP' TO ECR-REASON-CODE
           END-IF

      *    CHG3310 - MONEY SERVICES BUSINESSES DO NOT EARN CREDIT
           IF ECR-CUST-IS-MSB
               MOVE ZERO   TO ECR-RATE
               MOVE 'MSB ' TO ECR-REASON-CODE
           END-IF.

      *----------------------------------------------------------------*
      * EARNINGS CREDIT                                                *
      * RESERVE REQUIREMENT DEDUCTED PER REG D TRANSACTION ACCOUNT     *
      * RESERVE RATIO (10% OVER THE LOW RESERVE TRANCHE).              *
      *----------------------------------------------------------------*
       2000-COMPUTE-CREDIT.
           IF ECR-AVG-COLLECTED-BAL <= 0
               MOVE 04     TO ECR-RETURN-CODE
               MOVE 'NBAL' TO ECR-REASON-CODE
               EXIT PARAGRAPH
           END-IF

           COMPUTE ECR-INVESTABLE-BAL ROUNDED =
                   ECR-AVG-COLLECTED-BAL
                 * (100 - WS-RESERVE-REQ-PCT) / 100

           COMPUTE WS-CREDIT =
                   ECR-INVESTABLE-BAL * ECR-RATE / 100
                 * ECR-DAYS-IN-PERIOD / WS-DAY-BASIS
           COMPUTE ECR-EARNINGS-CREDIT ROUNDED = WS-CREDIT.
