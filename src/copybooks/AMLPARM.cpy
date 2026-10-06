      *================================================================*
      * COPYBOOK : AMLPARM                                             *
      * PURPOSE  : LINKAGE FOR AMLRULES - TRANSACTION MONITORING       *
      *            SCENARIO ENGINE FOR ONE CUSTOMER. CALLER PASSES THE *
      *            CUSTOMER PROFILE, THE CUSTOMER'S ACCOUNTS AND ALL   *
      *            POSTED ITEMS IN THE LOOKBACK WINDOW (INCL. TODAY).  *
      *            SCENARIO PARAMETERS COME FROM COPYBOOK AMLSCEN.     *
      *   RETURN CODE 00 = NO HITS, 04 = ONE OR MORE HITS,             *
      *               12 = INVALID INPUT                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2004-08-02 BHS  ORIGINAL - AML1 TO AML3                        *
      * 2011-01-18 DPS  AML4 MSB SPIKE, AML5 DORMANT REACTIVATION      *
      * 2015-05-27 DPS  AML6 ROUND WIRE NEW BENEFICIARY                *
      * 2018-05-11 AKW  CDD RULE - AML7 PROFILE MISMATCH               *
      *================================================================*
       01  AML-RULE-PARMS.
      *--- INPUT - PROFILE -------------------------------------------*
           05  AMP-BUSINESS-DATE           PIC 9(08).
           05  AMP-CUST-ID                 PIC X(10).
           05  AMP-CUST-TYPE               PIC X(01).
           05  AMP-SEGMENT                 PIC X(02).
           05  AMP-PEP-FLAG                PIC X(01).
           05  AMP-MSB-FLAG                PIC X(01).
           05  AMP-RISK-RATING             PIC X(01).
           05  AMP-ANNUAL-INCOME           PIC S9(13)V99 COMP-3.
           05  AMP-CUST-SINCE-DATE         PIC 9(08).
      *--- INPUT - ACCOUNTS ------------------------------------------*
           05  AMP-ACCT-COUNT              PIC 9(02).
           05  AMP-ACCT OCCURS 20 TIMES.
               10  AMP-ACCT-NUMBER         PIC X(12).
               10  AMP-ACCT-STATUS         PIC X(01).
               10  AMP-ACCT-OPEN-DATE      PIC 9(08).
               10  AMP-ACCT-DORMANT-DATE   PIC 9(08).
               10  AMP-ACCT-LAST-ACT-DATE  PIC 9(08).
      *--- INPUT - TRANSACTIONS (OLDEST FIRST) -----------------------*
           05  AMP-ITEM-COUNT              PIC 9(03).
           05  AMP-ITEM OCCURS 250 TIMES.
               10  AMP-ITEM-DATE           PIC 9(08).
               10  AMP-ITEM-ACCT           PIC X(12).
               10  AMP-ITEM-CODE           PIC X(04).
               10  AMP-ITEM-DR-CR          PIC X(01).
               10  AMP-ITEM-AMOUNT         PIC S9(13)V99 COMP-3.
               10  AMP-ITEM-CHANNEL        PIC X(03).
               10  AMP-ITEM-CASH-FLAG      PIC X(01).
               10  AMP-ITEM-CP-ACCT        PIC X(17).
               10  AMP-ITEM-CP-COUNTRY     PIC X(02).
      *--- OUTPUT ----------------------------------------------------*
           05  AMP-HIT-COUNT               PIC 9(02).
           05  AMP-HIT OCCURS 20 TIMES.
               10  AMP-HIT-SCENARIO        PIC X(04).
               10  AMP-HIT-RULE-ID         PIC X(08).
               10  AMP-HIT-SEVERITY        PIC X(01).
               10  AMP-HIT-ACCT            PIC X(12).
               10  AMP-HIT-AMOUNT          PIC S9(13)V99 COMP-3.
               10  AMP-HIT-DESCRIPTION     PIC X(80).
           05  AMP-RETURN-CODE             PIC 9(02).
