      *================================================================*
      * PROGRAM  : REGDCHK                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CONVENIENT WITHDRAWAL LIMIT FOR SAVINGS (SV) AND    *
      *            MONEY MARKET (MM) ACCOUNTS. THE FED REMOVED THE     *
      *            REG D SIX-PER-MONTH LIMIT IN APRIL 2020; THE BANK   *
      *            RETAINED IT AS POLICY (CHG3301):                    *
      *              - 6 CONVENIENT WITHDRAWALS PER STATEMENT CYCLE    *
      *              - 7TH AND LATER ARE STILL PAID, EXCESS FEE $10    *
      *                EACH (TRAN CODE WFRD)                           *
      *              - THE FIRST EXCESS IN A CYCLE IS A VIOLATION;     *
      *                3 OR MORE VIOLATIONS IN 12 MONTHS RAISES A      *
      *                RECOMMENDATION TO CONVERT THE ACCOUNT           *
      *              - ONLY TRAN CODES WITH TC-REGD-FLAG 'Y' COUNT     *
      *              - IN-PERSON (BRANCH / ATM) WITHDRAWALS EXCLUDED   *
      * LINKAGE  : RGDPARM                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - 6 PER MONTH, MAX 3 BY CHECK         *
      * 1998-11-02 MKL  Y2K REVIEW - NO CHANGE                         *
      * 2009-01-12 DPS  CHECK SUB-LIMIT REMOVED                        *
      * 2012-05-07 DPS  EXCESS FEE $10 (WAS $5) CHG1144                *
      * 2014-09-22 DPS  3 VIOLATIONS -> CONVERT (WAS CLOSE ACCOUNT)    *
      * 2020-05-01 AKW  CHG3301 FED INTERIM FINAL RULE - POLICY KEPT   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGDCHK.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-REGD-POLICY.
           05  WS-MAX-CONVENIENT-WD        PIC 9(03)     VALUE 006.
           05  WS-EXCESS-FEE               PIC S9(05)V99 VALUE 10.00.
      *    2012-05 DPS WAS 5.00
           05  WS-CONVERT-VIOLATIONS       PIC 9(02)     VALUE 03.

       LINKAGE SECTION.
       COPY RGDPARM.

       PROCEDURE DIVISION USING REGDCHK-PARMS.
       0000-MAIN.
           MOVE 'N'    TO RGD-COUNTED-FLAG RGD-EXCESS-FLAG
                          RGD-CONVERT-FLAG
           MOVE SPACES TO RGD-FEE-CODE RGD-REASON-CODE
           MOVE ZERO   TO RGD-FEE-AMOUNT
           MOVE 00     TO RGD-RETURN-CODE

           EVALUATE TRUE
               WHEN RGD-PROD-REGD-FLAG NOT = 'Y'
                 OR (RGD-ACCT-CATEGORY NOT = 'SV'
                     AND RGD-ACCT-CATEGORY NOT = 'MM')
                   MOVE 'NAPP' TO RGD-REASON-CODE
               WHEN RGD-TRAN-REGD-FLAG NOT = 'Y'
                   MOVE 'NCNT' TO RGD-REASON-CODE
               WHEN RGD-CHANNEL = 'BRN' OR RGD-CHANNEL = 'ATM'
                   MOVE 'INPR' TO RGD-REASON-CODE
               WHEN OTHER
                   PERFORM 1000-COUNT-WITHDRAWAL
           END-EVALUATE
           GOBACK.

       1000-COUNT-WITHDRAWAL.
           ADD 1 TO RGD-WD-COUNT
           MOVE 'Y' TO RGD-COUNTED-FLAG
           IF RGD-WD-COUNT > WS-MAX-CONVENIENT-WD
               MOVE 'Y'    TO RGD-EXCESS-FLAG
               MOVE 'EXCS' TO RGD-REASON-CODE
               MOVE 'WFRD' TO RGD-FEE-CODE
               MOVE WS-EXCESS-FEE TO RGD-FEE-AMOUNT
               IF RGD-WD-COUNT = WS-MAX-CONVENIENT-WD + 1
                   IF RGD-VIOLATIONS-12M < 99
                       ADD 1 TO RGD-VIOLATIONS-12M
                   END-IF
               END-IF
               IF RGD-VIOLATIONS-12M NOT < WS-CONVERT-VIOLATIONS
                   MOVE 'Y' TO RGD-CONVERT-FLAG
               END-IF
               MOVE 04 TO RGD-RETURN-CODE
           ELSE
               MOVE 'CNTD' TO RGD-REASON-CODE
           END-IF.
