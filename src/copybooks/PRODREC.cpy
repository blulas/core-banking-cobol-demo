      *================================================================*
      * COPYBOOK : PRODREC                                             *
      * PURPOSE  : PRODUCT MASTER - DEPOSIT AND LOAN PRODUCT DEFS      *
      * FILE     : PRODMST  (KSDS, KEY = PROD-CODE)                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2009-11-17 DPS  REG E OPT-IN, OD ALLOWED FLAG                  *
      * 2017-02-08 AKW  ONLINE SAVINGS / NO-PENALTY CD PRODUCTS        *
      *================================================================*
       01  PRODUCT-RECORD.
           05  PROD-CODE                   PIC X(04).
           05  PROD-NAME                   PIC X(30).
           05  PROD-CATEGORY               PIC X(02).
               88  PROD-SAVINGS                      VALUE 'SV'.
               88  PROD-DEMAND-DEPOSIT               VALUE 'DD'.
               88  PROD-TIME-DEPOSIT                 VALUE 'TD'.
               88  PROD-MONEY-MARKET                 VALUE 'MM'.
               88  PROD-TERM-LOAN                    VALUE 'LN'.
               88  PROD-LINE-OF-CREDIT               VALUE 'LC'.
               88  PROD-IS-DEPOSIT       VALUE 'SV' 'DD' 'TD' 'MM'.
               88  PROD-IS-LOAN          VALUE 'LN' 'LC'.
           05  PROD-CUST-TYPE-ALLOWED      PIC X(01).
               88  PROD-FOR-INDIVIDUAL               VALUE 'I'.
               88  PROD-FOR-CORPORATE                VALUE 'C'.
               88  PROD-FOR-ALL                      VALUE 'B'.
           05  PROD-MIN-OPEN-AMT           PIC S9(13)V99 COMP-3.
           05  PROD-MIN-BALANCE            PIC S9(13)V99 COMP-3.
           05  PROD-MAINT-FEE              PIC S9(05)V99 COMP-3.
           05  PROD-RATE-TABLE-ID          PIC X(04).
           05  PROD-TERM-MONTHS            PIC 9(03).
           05  PROD-ACCRUAL-BASIS          PIC X(01).
               88  PROD-ACTUAL-365                   VALUE '3'.
               88  PROD-ACTUAL-360                   VALUE 'A'.
               88  PROD-30-360                       VALUE '0'.
           05  PROD-COMPOUND-FREQ          PIC X(01).
               88  PROD-COMPOUND-DAILY               VALUE 'D'.
               88  PROD-COMPOUND-MONTHLY             VALUE 'M'.
               88  PROD-COMPOUND-NONE                VALUE 'N'.
           05  PROD-CAPITALIZE-FREQ        PIC X(01).
               88  PROD-CAP-MONTHLY                  VALUE 'M'.
               88  PROD-CAP-QUARTERLY                VALUE 'Q'.
               88  PROD-CAP-AT-MATURITY              VALUE 'T'.
               88  PROD-CAP-NONE                     VALUE 'N'.
           05  PROD-REG-D-FLAG             PIC X(01).
               88  PROD-REG-D-LIMITED                VALUE 'Y'.
           05  PROD-OD-ALLOWED             PIC X(01).
               88  PROD-ALLOWS-OD                    VALUE 'Y'.
           05  PROD-CHECK-ALLOWED          PIC X(01).
               88  PROD-ALLOWS-CHECKS                VALUE 'Y'.
           05  PROD-ANALYSIS-FLAG          PIC X(01).
               88  PROD-ACCOUNT-ANALYSIS             VALUE 'Y'.
      *    CD EARLY WITHDRAWAL PENALTY EXPRESSED AS DAYS OF INTEREST
           05  PROD-EARLY-WD-PENALTY-DAYS  PIC 9(03).
           05  PROD-STATUS                 PIC X(01).
               88  PROD-ACTIVE                       VALUE 'A'.
               88  PROD-RETIRED                      VALUE 'R'.
           05  PROD-EFF-DATE               PIC 9(08).
           05  FILLER                      PIC X(30).
