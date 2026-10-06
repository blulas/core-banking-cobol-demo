      *================================================================*
      * COPYBOOK : CRDTEP                                              *
      * PURPOSE  : LINKAGE FOR PROPRIETARY CREDIT CARD RULE MODULE     *
      *            CRDTE                                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-25 EWS  ORIGINAL                                       *
      * 1996-08-02 BHS  CASH-ADV STATUS CODES ADDED PRB7096            *
      * 1998-07-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-07-28 LMB  PURCHASE STATUS CODES ADDED PRB2257            *
      * 1999-01-03 MKL  NEW MIN-DUE REPORT COLUMNS INC3943             *
      * 1999-11-20 CRP  PAYMENT NOTICE TEXT PER LEGAL REQ1395          *
      * 1999-05-15 EWS  CASH-ADV TABLE EXPANDED REQ5506                *
      * 2005-11-14 MKL  LATE-FEE RERUN PROTECTION REQ7100              *
      * 2005-05-25 RJT  PERFORMANCE - LIMIT READ AHEAD PRB5718         *
      *================================================================*
       01  CRDTE-PARMS.
           05  CRDTE-PURCHASE-YTD          PIC S9(13)V99 COMP-3.
           05  CRDTE-LIMIT-AMT             PIC S9(13)V99 COMP-3.
           05  CRDTE-FINANCE-AMT           PIC S9(13)V99 COMP-3.
           05  CRDTE-LIMIT-YTD             PIC S9(13)V99 COMP-3.
           05  CRDTE-PURCHASE-AMT          PIC S9(13)V99 COMP-3.
           05  CRDTE-LATE-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  CRDTE-EXPIRY-DATE           PIC 9(08).
           05  CRDTE-OPEN-DATE             PIC 9(08).
           05  CRDTE-LAST-PAY-DATE         PIC 9(08).
           05  CRDTE-CLOSE-DATE            PIC 9(08).
           05  CRDTE-CARD-TYPE-CD          PIC X(03).
               88  CRDTE-CARD-TYPE-CD-PGU          VALUE 'PGU'.
               88  CRDTE-CARD-TYPE-CD-TRN          VALUE 'TRN'.
               88  CRDTE-CARD-TYPE-CD-WHR          VALUE 'WHR'.
               88  CRDTE-CARD-TYPE-CD-ZWB          VALUE 'ZWB'.
           05  CRDTE-SOURCE-CD             PIC X(01).
               88  CRDTE-SOURCE-CD-C               VALUE 'C'.
               88  CRDTE-SOURCE-CD-H               VALUE 'H'.
               88  CRDTE-SOURCE-CD-M               VALUE 'M'.
               88  CRDTE-SOURCE-CD-R               VALUE 'R'.
               88  CRDTE-SOURCE-CD-S               VALUE 'S'.
               88  CRDTE-SOURCE-CD-T               VALUE 'T'.
           05  CRDTE-TYPE-CD               PIC X(02).
               88  CRDTE-TYPE-CD-GP                VALUE 'GP'.
               88  CRDTE-TYPE-CD-UV                VALUE 'UV'.
               88  CRDTE-TYPE-CD-YK                VALUE 'YK'.
           05  CRDTE-BRANCH-CD             PIC X(03).
               88  CRDTE-BRANCH-CD-CTB             VALUE 'CTB'.
               88  CRDTE-BRANCH-CD-LNX             VALUE 'LNX'.
               88  CRDTE-BRANCH-CD-MGS             VALUE 'MGS'.
               88  CRDTE-BRANCH-CD-YNJ             VALUE 'YNJ'.
           05  CRDTE-PREFERRED-FLAG        PIC X(01).
               88  CRDTE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  CRDTE-STOP-FLAG             PIC X(01).
               88  CRDTE-STOP-FLAG-YES             VALUE 'Y'.
           05  CRDTE-EXEMPT-FLAG           PIC X(01).
               88  CRDTE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CRDTE-NOTICE-CNT            PIC 9(07).
           05  CRDTE-CYCLE-CNT             PIC 9(07).
           05  CRDTE-DISCOUNT              PIC 9(03)V9(06).
           05  CRDTE-PENALTY-PCT           PIC 9(03)V9(06).
           05  CRDTE-NAME                  PIC X(30).
           05  CRDTE-CUST-ID               PIC X(10).
           05  CRDTE-RETURN-CODE           PIC 9(02).
