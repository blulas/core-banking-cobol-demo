      *================================================================*
      * COPYBOOK : RCCPARM                                             *
      * PURPOSE  : LINKAGE FOR REGCCHLD - REG CC FUNDS AVAILABILITY    *
      *            FOR A DEPOSIT. RETURNS UP TO 3 HOLD TIERS, EACH     *
      *            WITH AMOUNT, TYPE AND RELEASE DATE.                 *
      *----------------------------------------------------------------*
      * EXCEPTION CODES (RCC-EXCEPTION-CODE)                           *
      *   NEWA  NEW ACCOUNT (OPEN < 30 CALENDAR DAYS)                  *
      *   RPOD  REPEATED OVERDRAFTS (NSF-12M >= 6)                     *
      *   LRGD  LARGE DEPOSIT (DAY'S CHECKS > $6,725)                  *
      * RETURN CODE: 00 NO HOLD, 04 HOLD(S) RETURNED, 12 BAD DATE      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 RJT  ORIGINAL - EXPEDITED FUNDS AVAILABILITY ACT    *
      * 2011-07-21 DPS  EXCEPTION HOLD TIERS                           *
      * 2025-07-01 AKW  REG-2025-CC INFLATION ADJUSTED AMOUNTS         *
      *================================================================*
       01  REGCCHLD-PARMS.
           05  RCC-INPUT.
               10  RCC-TRAN-CODE           PIC X(04).
               10  RCC-CHANNEL             PIC X(03).
               10  RCC-AMOUNT              PIC S9(13)V99 COMP-3.
      *        CHECKS ALREADY DEPOSITED TO THIS ACCOUNT TODAY
               10  RCC-DEPOSITS-TODAY      PIC S9(13)V99 COMP-3.
               10  RCC-BUSINESS-DATE       PIC 9(08).
               10  RCC-ACCT-OPEN-DATE      PIC 9(08).
               10  RCC-NSF-COUNT-12M       PIC 9(03).
               10  RCC-CUST-SEGMENT        PIC X(02).
           05  RCC-OUTPUT.
               10  RCC-HOLD-COUNT          PIC 9(01).
               10  RCC-TOTAL-HOLD-AMT      PIC S9(13)V99 COMP-3.
               10  RCC-MAX-HOLD-DAYS       PIC 9(02).
               10  RCC-EXCEPTION-CODE      PIC X(04).
               10  RCC-HOLD-TIER OCCURS 3 TIMES.
                   15  RCC-HOLD-TYPE       PIC X(02).
                   15  RCC-HOLD-AMT        PIC S9(13)V99 COMP-3.
                   15  RCC-HOLD-BUS-DAYS   PIC 9(02).
                   15  RCC-RELEASE-DATE    PIC 9(08).
                   15  RCC-HOLD-REASON     PIC X(30).
           05  RCC-RETURN-CODE             PIC 9(02).
