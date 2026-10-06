      *================================================================*
      * COPYBOOK : SBPENP                                              *
      * PURPOSE  : LINKAGE FOR SAVINGS BOND SERVICES RULE MODULE SBPEN *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-07 RJT  ORIGINAL                                       *
      * 1996-11-09 LMB  TAXABLE CALCULATION CORRECTED INC0599          *
      * 1997-08-24 GAF  NEW REDEEM REPORT COLUMNS INC6079              *
      * 1998-07-03 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-05-16 MKL  FACE STATUS CODES ADDED PRB1610                *
      * 2003-05-01 CRP  REDEEM CALCULATION CORRECTED INC5677           *
      * 2009-11-15 PQR  FACE CALCULATION CORRECTED REQ0181             *
      *================================================================*
       01  SBPEN-PARMS.
           05  SBPEN-ACCRUAL-YTD           PIC S9(13)V99 COMP-3.
           05  SBPEN-PURCHASE-YTD          PIC S9(13)V99 COMP-3.
           05  SBPEN-PURCHASE-AMT          PIC S9(13)V99 COMP-3.
           05  SBPEN-REDEEM-AMT            PIC S9(13)V99 COMP-3.
           05  SBPEN-ACCRUAL-AMT           PIC S9(13)V99 COMP-3.
           05  SBPEN-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  SBPEN-NOTICE-DATE           PIC 9(08).
           05  SBPEN-LAST-MAINT-DATE       PIC 9(08).
           05  SBPEN-NEXT-REVIEW-DATE      PIC 9(08).
           05  SBPEN-NEXT-DUE-DATE         PIC 9(08).
           05  SBPEN-STATUS-CD             PIC X(02).
               88  SBPEN-STATUS-CD-CS              VALUE 'CS'.
               88  SBPEN-STATUS-CD-HF              VALUE 'HF'.
               88  SBPEN-STATUS-CD-JZ              VALUE 'JZ'.
           05  SBPEN-SERIES-CD             PIC X(02).
           05  SBPEN-BRANCH-CD             PIC X(02).
           05  SBPEN-SOURCE-CD             PIC X(01).
               88  SBPEN-SOURCE-CD-E               VALUE 'E'.
               88  SBPEN-SOURCE-CD-J               VALUE 'J'.
               88  SBPEN-SOURCE-CD-L               VALUE 'L'.
               88  SBPEN-SOURCE-CD-U               VALUE 'U'.
           05  SBPEN-PREFERRED-FLAG        PIC X(01).
               88  SBPEN-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  SBPEN-HOLD-FLAG             PIC X(01).
               88  SBPEN-HOLD-FLAG-YES             VALUE 'Y'.
           05  SBPEN-OVERRIDE-FLAG         PIC X(01).
               88  SBPEN-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  SBPEN-PAYMENT-CNT           PIC 9(07).
           05  SBPEN-CYCLE-CNT             PIC 9(07).
           05  SBPEN-MARGIN                PIC 9(03)V9(06).
           05  SBPEN-FEE-PCT               PIC 9(03)V9(06).
           05  SBPEN-REF-NO                PIC X(16).
           05  SBPEN-ADDR                  PIC X(30).
           05  SBPEN-RETURN-CODE           PIC 9(02).
