      *================================================================*
      * COPYBOOK : CRTIERP                                             *
      * PURPOSE  : LINKAGE FOR PROPRIETARY CREDIT CARD RULE MODULE     *
      *            CRTIER                                              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-21 RJT  ORIGINAL                                       *
      * 1998-08-14 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-05-01 GAF  FINANCE EXCEPTIONS TO OPS QUEUE REQ8860        *
      * 2002-05-17 TWH  CASH-ADV TABLE EXPANDED CHG6778                *
      * 2002-05-18 LMB  AUDIT FINDING - CASH-ADV CONTROL TOTALS PRB1509*
      * 2003-03-28 PQR  PURCHASE STATUS CODES ADDED PRB3730            *
      *================================================================*
       01  CRTIER-PARMS.
           05  CRTIER-PAYMENT-AMT          PIC S9(13)V99 COMP-3.
           05  CRTIER-LATE-FEE-AMT         PIC S9(13)V99 COMP-3.
           05  CRTIER-FINANCE-YTD          PIC S9(13)V99 COMP-3.
           05  CRTIER-CASH-ADV-YTD         PIC S9(13)V99 COMP-3.
           05  CRTIER-LATE-FEE-YTD         PIC S9(13)V99 COMP-3.
           05  CRTIER-LIMIT-AMT            PIC S9(13)V99 COMP-3.
           05  CRTIER-EXPIRY-DATE          PIC 9(08).
           05  CRTIER-MATURITY-DATE        PIC 9(08).
           05  CRTIER-LAST-PAY-DATE        PIC 9(08).
           05  CRTIER-OPEN-DATE            PIC 9(08).
           05  CRTIER-SOURCE-CD            PIC X(01).
               88  CRTIER-SOURCE-CD-C              VALUE 'C'.
               88  CRTIER-SOURCE-CD-H              VALUE 'H'.
               88  CRTIER-SOURCE-CD-M              VALUE 'M'.
               88  CRTIER-SOURCE-CD-R              VALUE 'R'.
               88  CRTIER-SOURCE-CD-S              VALUE 'S'.
               88  CRTIER-SOURCE-CD-T              VALUE 'T'.
           05  CRTIER-STATUS-CD            PIC X(03).
               88  CRTIER-STATUS-CD-BAL            VALUE 'BAL'.
               88  CRTIER-STATUS-CD-DDY            VALUE 'DDY'.
               88  CRTIER-STATUS-CD-DPB            VALUE 'DPB'.
               88  CRTIER-STATUS-CD-UHF            VALUE 'UHF'.
               88  CRTIER-STATUS-CD-WLL            VALUE 'WLL'.
               88  CRTIER-STATUS-CD-XAW            VALUE 'XAW'.
           05  CRTIER-BRANCH-CD            PIC X(03).
               88  CRTIER-BRANCH-CD-CTB            VALUE 'CTB'.
               88  CRTIER-BRANCH-CD-LNX            VALUE 'LNX'.
               88  CRTIER-BRANCH-CD-MGS            VALUE 'MGS'.
               88  CRTIER-BRANCH-CD-YNJ            VALUE 'YNJ'.
           05  CRTIER-CARD-TYPE-CD         PIC X(03).
               88  CRTIER-CARD-TYPE-CD-PGU         VALUE 'PGU'.
               88  CRTIER-CARD-TYPE-CD-TRN         VALUE 'TRN'.
               88  CRTIER-CARD-TYPE-CD-WHR         VALUE 'WHR'.
               88  CRTIER-CARD-TYPE-CD-ZWB         VALUE 'ZWB'.
           05  CRTIER-NOTICE-SENT-FLAG     PIC X(01).
               88  CRTIER-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  CRTIER-PREFERRED-FLAG       PIC X(01).
               88  CRTIER-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  CRTIER-AUTO-RENEW-FLAG      PIC X(01).
               88  CRTIER-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  CRTIER-CYCLE-CNT            PIC 9(07).
           05  CRTIER-RETRY-CNT            PIC 9(07).
           05  CRTIER-DISCOUNT             PIC 9(03)V9(06).
           05  CRTIER-PENALTY-PCT          PIC 9(03)V9(06).
           05  CRTIER-NAME                 PIC X(30).
           05  CRTIER-DESC                 PIC X(30).
           05  CRTIER-RETURN-CODE          PIC 9(02).
