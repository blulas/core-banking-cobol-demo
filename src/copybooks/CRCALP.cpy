      *================================================================*
      * COPYBOOK : CRCALP                                              *
      * PURPOSE  : LINKAGE FOR PROPRIETARY CREDIT CARD RULE MODULE     *
      *            CRCAL                                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-22 CRP  ORIGINAL                                       *
      * 1997-08-22 MKL  FINANCE CALCULATION CORRECTED REQ3213          *
      * 1998-01-17 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-11 TWH  PURCHASE TABLE EXPANDED PRB7613                *
      * 1999-12-20 TWH  MIN-DUE TABLE EXPANDED INC9675                 *
      * 1999-07-12 LMB  LATE-FEE TABLE EXPANDED CHG9168                *
      * 2002-09-14 DPS  NEW PURCHASE REPORT COLUMNS PRB8753            *
      * 2004-07-22 MKL  LIMIT RERUN PROTECTION REQ1064                 *
      * 2004-03-10 RJT  FINANCE STATUS CODES ADDED PRB0429             *
      * 2004-04-14 LMB  PAYMENT EXCEPTIONS TO OPS QUEUE REQ2045        *
      * 2004-05-09 DPS  MIN-DUE LIMIT REVISED CHG6519                  *
      * 2005-10-20 GAF  MIN-DUE ROUNDING - HALF UP INC0480             *
      * 2006-04-26 RJT  FINANCE RERUN PROTECTION CHG1706               *
      *================================================================*
       01  CRCAL-PARMS.
           05  CRCAL-LIMIT-AMT             PIC S9(13)V99 COMP-3.
           05  CRCAL-CASH-ADV-AMT          PIC S9(13)V99 COMP-3.
           05  CRCAL-MIN-DUE-YTD           PIC S9(13)V99 COMP-3.
           05  CRCAL-LATE-FEE-AMT          PIC S9(13)V99 COMP-3.
           05  CRCAL-FINANCE-YTD           PIC S9(13)V99 COMP-3.
           05  CRCAL-CASH-ADV-YTD          PIC S9(13)V99 COMP-3.
           05  CRCAL-EFFECTIVE-DATE        PIC 9(08).
           05  CRCAL-CLOSE-DATE            PIC 9(08).
           05  CRCAL-MATURITY-DATE         PIC 9(08).
           05  CRCAL-EXPIRY-DATE           PIC 9(08).
           05  CRCAL-STATUS-CD             PIC X(03).
               88  CRCAL-STATUS-CD-BAL             VALUE 'BAL'.
               88  CRCAL-STATUS-CD-DDY             VALUE 'DDY'.
               88  CRCAL-STATUS-CD-DPB             VALUE 'DPB'.
               88  CRCAL-STATUS-CD-UHF             VALUE 'UHF'.
               88  CRCAL-STATUS-CD-WLL             VALUE 'WLL'.
               88  CRCAL-STATUS-CD-XAW             VALUE 'XAW'.
           05  CRCAL-SOURCE-CD             PIC X(01).
               88  CRCAL-SOURCE-CD-C               VALUE 'C'.
               88  CRCAL-SOURCE-CD-H               VALUE 'H'.
               88  CRCAL-SOURCE-CD-M               VALUE 'M'.
               88  CRCAL-SOURCE-CD-R               VALUE 'R'.
               88  CRCAL-SOURCE-CD-S               VALUE 'S'.
               88  CRCAL-SOURCE-CD-T               VALUE 'T'.
           05  CRCAL-CARD-TYPE-CD          PIC X(03).
               88  CRCAL-CARD-TYPE-CD-PGU          VALUE 'PGU'.
               88  CRCAL-CARD-TYPE-CD-TRN          VALUE 'TRN'.
               88  CRCAL-CARD-TYPE-CD-WHR          VALUE 'WHR'.
               88  CRCAL-CARD-TYPE-CD-ZWB          VALUE 'ZWB'.
           05  CRCAL-TYPE-CD               PIC X(02).
               88  CRCAL-TYPE-CD-GP                VALUE 'GP'.
               88  CRCAL-TYPE-CD-UV                VALUE 'UV'.
               88  CRCAL-TYPE-CD-YK                VALUE 'YK'.
           05  CRCAL-EXEMPT-FLAG           PIC X(01).
               88  CRCAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CRCAL-OVERRIDE-FLAG         PIC X(01).
               88  CRCAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CRCAL-AUTO-RENEW-FLAG       PIC X(01).
               88  CRCAL-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  CRCAL-RETRY-CNT             PIC 9(07).
           05  CRCAL-CYCLE-CNT             PIC 9(07).
           05  CRCAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  CRCAL-RATE                  PIC 9(03)V9(06).
           05  CRCAL-DESC                  PIC X(30).
           05  CRCAL-CUST-ID               PIC X(10).
           05  CRCAL-RETURN-CODE           PIC 9(02).
