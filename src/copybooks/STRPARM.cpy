      *================================================================*
      * COPYBOOK : STRPARM                                             *
      * PURPOSE  : LINKAGE FOR STRCRULE - STRUCTURING DETECTION FOR    *
      *            ONE CUSTOMER. CALLER PASSES ALL CASH ITEMS FOR THE  *
      *            CUSTOMER IN THE LOOKBACK WINDOW INCLUDING TODAY.    *
      *   PATTERN CODES (STP-PATTERN-CODES, UP TO 4 X 2 CHARS)         *
      *     S1  DEPOSITS 8,000-10,000 ON 2+ BUSINESS DAYS IN 10 DAYS   *
      *     S2  5-DAY ROLLING CASH > 10,000, NO SINGLE DAY > 10,000    *
      *     S3  SAME-DAY SUB-THRESHOLD CASH ACROSS BRANCHES            *
      *     S4  3+ ROUND-DOLLAR CASH DEPOSITS >= 3,000 IN 7 DAYS       *
      *   RETURN CODE 00 = NO PATTERN, 04 = PATTERN(S) FOUND,          *
      *               12 = INVALID INPUT                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-02-17 BHS  ORIGINAL (S1, S2)                              *
      * 2009-06-08 DPS  S3 MULTI-BRANCH, S4 ROUND DOLLAR (OCC MRA)     *
      *================================================================*
       01  STRC-RULE-PARMS.
      *--- INPUT -----------------------------------------------------*
           05  STP-BUSINESS-DATE           PIC 9(08).
           05  STP-CUST-ID                 PIC X(10).
           05  STP-ITEM-COUNT              PIC 9(03).
           05  STP-ITEM OCCURS 200 TIMES.
               10  STP-ITEM-DATE           PIC 9(08).
               10  STP-ITEM-DR-CR          PIC X(01).
               10  STP-ITEM-AMOUNT         PIC S9(13)V99 COMP-3.
               10  STP-ITEM-BRANCH         PIC X(05).
               10  STP-ITEM-ACCT           PIC X(12).
      *--- OUTPUT ----------------------------------------------------*
           05  STP-PATTERN-COUNT           PIC 9(01).
           05  STP-PATTERN-CODES           PIC X(08).
           05  STP-PATTERN-TBL REDEFINES STP-PATTERN-CODES.
               10  STP-PATTERN-CODE        PIC X(02) OCCURS 4 TIMES.
           05  STP-SCORE                   PIC 9(03).
           05  STP-SEVERITY                PIC X(01).
           05  STP-BAND-DAYS               PIC 9(02).
           05  STP-ROLLING-CASH-IN         PIC S9(13)V99 COMP-3.
           05  STP-ROLLING-CASH-OUT        PIC S9(13)V99 COMP-3.
           05  STP-TODAY-BRANCHES          PIC 9(02).
           05  STP-ROUND-COUNT             PIC 9(02).
           05  STP-SUSPECT-AMOUNT          PIC S9(13)V99 COMP-3.
           05  STP-DESCRIPTION             PIC X(80).
           05  STP-RETURN-CODE             PIC 9(02).
