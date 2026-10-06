      *================================================================*
      * COPYBOOK : FNELGP                                              *
      * PURPOSE  : LINKAGE FOR FOREIGN BANKNOTES RULE MODULE FNELG     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-07 CRP  ORIGINAL                                       *
      * 1996-11-24 DPS  NEW FOREIGN REPORT COLUMNS CHG4066             *
      * 1998-04-24 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-05 CRP  USD EXCEPTIONS TO OPS QUEUE INC0427            *
      * 1998-02-09 PQR  USD LIMIT REVISED INC2375                      *
      * 1999-07-03 BHS  USD LIMIT REVISED INC1952                      *
      * 2000-09-07 MKL  PERFORMANCE - MARGIN READ AHEAD REQ9614        *
      * 2000-09-11 BHS  BUYBACK FEE SCHEDULE UPDATE PRB7961            *
      * 2001-12-14 BHS  MARGIN NOTICE TEXT PER LEGAL PRB0402           *
      * 2005-02-10 GAF  MARGIN LIMIT REVISED REQ1195                   *
      * 2005-02-15 DPS  BUYBACK CALCULATION CORRECTED INC7016          *
      * 2005-03-24 SKN  SHIPPING ROUNDING - HALF UP CHG8167            *
      *================================================================*
       01  FNELG-PARMS.
           05  FNELG-FOREIGN-AMT           PIC S9(13)V99 COMP-3.
           05  FNELG-SHIPPING-AMT          PIC S9(13)V99 COMP-3.
           05  FNELG-MARGIN-YTD            PIC S9(13)V99 COMP-3.
           05  FNELG-USD-AMT               PIC S9(13)V99 COMP-3.
           05  FNELG-MARGIN-AMT            PIC S9(13)V99 COMP-3.
           05  FNELG-BUYBACK-AMT           PIC S9(13)V99 COMP-3.
           05  FNELG-NOTICE-DATE           PIC 9(08).
           05  FNELG-LAST-MAINT-DATE       PIC 9(08).
           05  FNELG-OPEN-DATE             PIC 9(08).
           05  FNELG-LAST-PAY-DATE         PIC 9(08).
           05  FNELG-STATUS-CD             PIC X(03).
               88  FNELG-STATUS-CD-CXH             VALUE 'CXH'.
               88  FNELG-STATUS-CD-ESS             VALUE 'ESS'.
               88  FNELG-STATUS-CD-XDE             VALUE 'XDE'.
               88  FNELG-STATUS-CD-XFD             VALUE 'XFD'.
           05  FNELG-ORDER-TYPE-CD         PIC X(03).
               88  FNELG-ORDER-TYPE-CD-DTU         VALUE 'DTU'.
               88  FNELG-ORDER-TYPE-CD-KZV         VALUE 'KZV'.
               88  FNELG-ORDER-TYPE-CD-PHV         VALUE 'PHV'.
               88  FNELG-ORDER-TYPE-CD-PZZ         VALUE 'PZZ'.
               88  FNELG-ORDER-TYPE-CD-WMC         VALUE 'WMC'.
               88  FNELG-ORDER-TYPE-CD-XWF         VALUE 'XWF'.
           05  FNELG-BRANCH-CD             PIC X(03).
               88  FNELG-BRANCH-CD-APZ             VALUE 'APZ'.
               88  FNELG-BRANCH-CD-BBP             VALUE 'BBP'.
               88  FNELG-BRANCH-CD-ENL             VALUE 'ENL'.
               88  FNELG-BRANCH-CD-EZB             VALUE 'EZB'.
               88  FNELG-BRANCH-CD-KHV             VALUE 'KHV'.
               88  FNELG-BRANCH-CD-TDN             VALUE 'TDN'.
           05  FNELG-SOURCE-CD             PIC X(02).
           05  FNELG-OVERRIDE-FLAG         PIC X(01).
               88  FNELG-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  FNELG-PREFERRED-FLAG        PIC X(01).
               88  FNELG-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  FNELG-REVIEW-FLAG           PIC X(01).
               88  FNELG-REVIEW-FLAG-YES           VALUE 'Y'.
           05  FNELG-PAYMENT-CNT           PIC 9(07).
           05  FNELG-CYCLE-CNT             PIC 9(07).
           05  FNELG-RATE                  PIC 9(03)V9(06).
           05  FNELG-PENALTY-PCT           PIC 9(03)V9(06).
           05  FNELG-DESC                  PIC X(30).
           05  FNELG-ADDR                  PIC X(30).
           05  FNELG-RETURN-CODE           PIC 9(02).
