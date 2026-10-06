      *================================================================*
      * COPYBOOK : RGDPARM                                             *
      * PURPOSE  : LINKAGE FOR REGDCHK - CONVENIENT WITHDRAWAL LIMIT   *
      *            (FORMER REG D 204.2(D)(2), RETAINED AS BANK POLICY) *
      *            FOR SAVINGS AND MONEY MARKET ACCOUNTS.              *
      *----------------------------------------------------------------*
      * REASON CODES (RGD-REASON-CODE)                                 *
      *   NAPP  PRODUCT NOT REG D LIMITED                              *
      *   NCNT  TRAN CODE NOT A CONVENIENT WITHDRAWAL                  *
      *   INPR  IN-PERSON (BRANCH / ATM) - EXCLUDED                    *
      *   CNTD  COUNTED, WITHIN LIMIT                                  *
      *   EXCS  COUNTED, EXCEEDS LIMIT - EXCESS FEE                    *
      * RETURN CODE: 00 OK, 04 EXCESS WITHDRAWAL (FEE / ALERT)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - 6 PER MONTH, 3 BY CHECK             *
      * 2009-01-12 DPS  CHECK SUB-LIMIT REMOVED (FRB 2009 AMENDMENT)   *
      * 2020-05-01 AKW  FED SUSPENDED LIMIT - BANK POLICY RETAINED,    *
      *                 EXCESS NO LONGER CLOSES ACCOUNT, FEE ONLY      *
      *================================================================*
       01  REGDCHK-PARMS.
           05  RGD-INPUT.
               10  RGD-ACCT-CATEGORY       PIC X(02).
               10  RGD-PROD-REGD-FLAG      PIC X(01).
               10  RGD-TRAN-CODE           PIC X(04).
               10  RGD-TRAN-REGD-FLAG      PIC X(01).
               10  RGD-CHANNEL             PIC X(03).
      *    IN/OUT - UPDATED WHEN THE WITHDRAWAL IS COUNTED
           05  RGD-WD-COUNT                PIC 9(03).
           05  RGD-VIOLATIONS-12M          PIC 9(02).
           05  RGD-OUTPUT.
               10  RGD-COUNTED-FLAG        PIC X(01).
               10  RGD-EXCESS-FLAG         PIC X(01).
                   88  RGD-IS-EXCESS                 VALUE 'Y'.
               10  RGD-FEE-CODE            PIC X(04).
               10  RGD-FEE-AMOUNT          PIC S9(07)V99 COMP-3.
      *        Y = RECOMMEND CONVERSION TO A TRANSACTION ACCOUNT
               10  RGD-CONVERT-FLAG        PIC X(01).
                   88  RGD-RECOMMEND-CONVERT         VALUE 'Y'.
               10  RGD-REASON-CODE         PIC X(04).
           05  RGD-RETURN-CODE             PIC 9(02).
