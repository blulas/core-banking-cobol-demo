      *================================================================*
      * COPYBOOK : SDWAIVP                                             *
      * PURPOSE  : LINKAGE FOR SAFE DEPOSIT BOXES RULE MODULE SDWAIV   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-13 BHS  ORIGINAL                                       *
      * 1995-06-13 CRP  NEW KEY-FEE REPORT COLUMNS INC2841             *
      * 2002-10-04 JLM  RENT CUTOFF MOVED REQ9478                      *
      * 2003-07-12 CRP  DRILL RERUN PROTECTION CHG2603                 *
      * 2004-07-18 DPS  KEY-FEE CALCULATION CORRECTED PRB9452          *
      * 2004-01-10 EWS  RENT LIMIT REVISED PRB6428                     *
      * 2005-02-27 SKN  KEY-FEE ROUNDING - HALF UP REQ5869             *
      *================================================================*
       01  SDWAIV-PARMS.
           05  SDWAIV-DEPOSIT-YTD          PIC S9(13)V99 COMP-3.
           05  SDWAIV-DRILL-AMT            PIC S9(13)V99 COMP-3.
           05  SDWAIV-RENT-YTD             PIC S9(13)V99 COMP-3.
           05  SDWAIV-LATE-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  SDWAIV-DRILL-YTD            PIC S9(13)V99 COMP-3.
           05  SDWAIV-KEY-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  SDWAIV-NEXT-DUE-DATE        PIC 9(08).
           05  SDWAIV-OPEN-DATE            PIC 9(08).
           05  SDWAIV-MATURITY-DATE        PIC 9(08).
           05  SDWAIV-CLOSE-DATE           PIC 9(08).
           05  SDWAIV-VAULT-CD             PIC X(02).
               88  SDWAIV-VAULT-CD-BU              VALUE 'BU'.
               88  SDWAIV-VAULT-CD-ET              VALUE 'ET'.
               88  SDWAIV-VAULT-CD-FF              VALUE 'FF'.
               88  SDWAIV-VAULT-CD-NG              VALUE 'NG'.
               88  SDWAIV-VAULT-CD-NH              VALUE 'NH'.
           05  SDWAIV-SOURCE-CD            PIC X(02).
               88  SDWAIV-SOURCE-CD-DD             VALUE 'DD'.
               88  SDWAIV-SOURCE-CD-JZ             VALUE 'JZ'.
               88  SDWAIV-SOURCE-CD-PS             VALUE 'PS'.
               88  SDWAIV-SOURCE-CD-UM             VALUE 'UM'.
           05  SDWAIV-BOX-SIZE-CD          PIC X(02).
               88  SDWAIV-BOX-SIZE-CD-CU           VALUE 'CU'.
               88  SDWAIV-BOX-SIZE-CD-MG           VALUE 'MG'.
               88  SDWAIV-BOX-SIZE-CD-VG           VALUE 'VG'.
               88  SDWAIV-BOX-SIZE-CD-VN           VALUE 'VN'.
               88  SDWAIV-BOX-SIZE-CD-XJ           VALUE 'XJ'.
           05  SDWAIV-BRANCH-CD            PIC X(03).
               88  SDWAIV-BRANCH-CD-EXS            VALUE 'EXS'.
               88  SDWAIV-BRANCH-CD-FJW            VALUE 'FJW'.
               88  SDWAIV-BRANCH-CD-YFB            VALUE 'YFB'.
           05  SDWAIV-SUSPEND-FLAG         PIC X(01).
               88  SDWAIV-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  SDWAIV-REVIEW-FLAG          PIC X(01).
               88  SDWAIV-REVIEW-FLAG-YES          VALUE 'Y'.
           05  SDWAIV-OVERRIDE-FLAG        PIC X(01).
               88  SDWAIV-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  SDWAIV-PAYMENT-CNT          PIC 9(07).
           05  SDWAIV-NOTICE-CNT           PIC 9(07).
           05  SDWAIV-PENALTY-PCT          PIC 9(03)V9(06).
           05  SDWAIV-FEE-PCT              PIC 9(03)V9(06).
           05  SDWAIV-CUST-ID              PIC X(10).
           05  SDWAIV-NAME                 PIC X(30).
           05  SDWAIV-RETURN-CODE          PIC 9(02).
