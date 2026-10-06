      *================================================================*
      * COPYBOOK : BUSTATP                                             *
      * PURPOSE  : LINKAGE FOR BUMP-UP CERTIFICATES RULE MODULE BUSTAT *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-24 CRP  ORIGINAL                                       *
      * 1995-11-18 TWH  PRINCIPAL TABLE EXPANDED CHG4817               *
      * 1996-09-14 DPS  PENALTY LIMIT REVISED PRB5841                  *
      * 2001-10-26 TWH  INTEREST STATUS CODES ADDED PRB2838            *
      * 2001-03-12 MKL  PENALTY NOTICE TEXT PER LEGAL CHG4745          *
      * 2005-01-26 MKL  BUMP TABLE EXPANDED PRB2847                    *
      *================================================================*
       01  BUSTAT-PARMS.
           05  BUSTAT-PENALTY-YTD          PIC S9(13)V99 COMP-3.
           05  BUSTAT-ADDON-AMT            PIC S9(13)V99 COMP-3.
           05  BUSTAT-BUMP-AMT             PIC S9(13)V99 COMP-3.
           05  BUSTAT-PENALTY-AMT          PIC S9(13)V99 COMP-3.
           05  BUSTAT-PRINCIPAL-AMT        PIC S9(13)V99 COMP-3.
           05  BUSTAT-INTEREST-YTD         PIC S9(13)V99 COMP-3.
           05  BUSTAT-LAST-PAY-DATE        PIC 9(08).
           05  BUSTAT-EXPIRY-DATE          PIC 9(08).
           05  BUSTAT-MATURITY-DATE        PIC 9(08).
           05  BUSTAT-LAST-STMT-DATE       PIC 9(08).
           05  BUSTAT-ELECTION-CD          PIC X(01).
               88  BUSTAT-ELECTION-CD-L            VALUE 'L'.
               88  BUSTAT-ELECTION-CD-M            VALUE 'M'.
               88  BUSTAT-ELECTION-CD-N            VALUE 'N'.
               88  BUSTAT-ELECTION-CD-R            VALUE 'R'.
               88  BUSTAT-ELECTION-CD-V            VALUE 'V'.
           05  BUSTAT-CHANNEL-CD           PIC X(02).
               88  BUSTAT-CHANNEL-CD-EK            VALUE 'EK'.
               88  BUSTAT-CHANNEL-CD-JE            VALUE 'JE'.
               88  BUSTAT-CHANNEL-CD-PN            VALUE 'PN'.
               88  BUSTAT-CHANNEL-CD-WV            VALUE 'WV'.
           05  BUSTAT-SOURCE-CD            PIC X(02).
               88  BUSTAT-SOURCE-CD-AG             VALUE 'AG'.
               88  BUSTAT-SOURCE-CD-BF             VALUE 'BF'.
               88  BUSTAT-SOURCE-CD-HF             VALUE 'HF'.
               88  BUSTAT-SOURCE-CD-HR             VALUE 'HR'.
               88  BUSTAT-SOURCE-CD-UF             VALUE 'UF'.
               88  BUSTAT-SOURCE-CD-YD             VALUE 'YD'.
           05  BUSTAT-STEP-PLAN-CD         PIC X(03).
           05  BUSTAT-OVERRIDE-FLAG        PIC X(01).
               88  BUSTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  BUSTAT-REVIEW-FLAG          PIC X(01).
               88  BUSTAT-REVIEW-FLAG-YES          VALUE 'Y'.
           05  BUSTAT-STOP-FLAG            PIC X(01).
               88  BUSTAT-STOP-FLAG-YES            VALUE 'Y'.
           05  BUSTAT-CYCLE-CNT            PIC 9(07).
           05  BUSTAT-ITEM-CNT             PIC 9(07).
           05  BUSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  BUSTAT-RATE                 PIC 9(03)V9(06).
           05  BUSTAT-ADDR                 PIC X(30).
           05  BUSTAT-NAME                 PIC X(30).
           05  BUSTAT-RETURN-CODE          PIC 9(02).
