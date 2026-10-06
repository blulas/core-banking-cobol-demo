      *================================================================*
      * COPYBOOK : ACCTREC                                             *
      * PURPOSE  : DEPOSIT ACCOUNT MASTER RECORD                       *
      * FILE     : ACCTMST  (KSDS, KEY = ACCT-NUMBER,                  *
      *                      AIX  = ACCT-PRIMARY-CUST-ID WITH DUPS)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1993-04-19 RJT  AVAILABLE BALANCE / HOLDS                      *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  CASH IN/OUT TODAY FOR CTR AGGREGATION          *
      * 2009-11-17 DPS  REG E OD OPT-IN                                *
      * 2011-04-12 DPS  SWEEP / ZBA PARENT ACCOUNT                     *
      * 2014-01-06 DPS  UNCLAIMED PROPERTY (ESCHEAT) STATE             *
      * 2019-03-25 AKW  VIRTUAL ACCOUNTS                               *
      * 2020-05-01 AKW  REG D - FED REMOVED 6 LIMIT, BANK POLICY KEPT  *
      *================================================================*
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER                 PIC X(12).
           05  ACCT-PRODUCT-CODE           PIC X(04).
           05  ACCT-CATEGORY               PIC X(02).
           05  ACCT-PRIMARY-CUST-ID        PIC X(10).
           05  ACCT-JOINT-CUST-ID          PIC X(10).
           05  ACCT-OWNERSHIP-TYPE         PIC X(01).
               88  ACCT-OWN-SINGLE                   VALUE 'S'.
               88  ACCT-OWN-JOINT                    VALUE 'J'.
               88  ACCT-OWN-TRUST                    VALUE 'T'.
               88  ACCT-OWN-CORPORATE                VALUE 'C'.
           05  ACCT-COST-CENTER            PIC X(05).
           05  ACCT-CURRENCY               PIC X(03).
           05  ACCT-STATUS                 PIC X(01).
               88  ACCT-ACTIVE                       VALUE 'A'.
               88  ACCT-DORMANT                      VALUE 'D'.
               88  ACCT-FROZEN                       VALUE 'F'.
               88  ACCT-CLOSED                       VALUE 'C'.
               88  ACCT-PENDING-OPEN                 VALUE 'P'.
               88  ACCT-ESCHEATED                    VALUE 'E'.
               88  ACCT-RESTRICTED                   VALUE 'R'.
               88  ACCT-CHARGED-OFF                  VALUE 'X'.
           05  ACCT-RESTRICT-REASON        PIC X(02).
           05  ACCT-OPEN-DATE              PIC 9(08).
           05  ACCT-CLOSE-DATE             PIC 9(08).
      *    LAST CUSTOMER-INITIATED ACTIVITY (DRIVES DORMANCY)
           05  ACCT-LAST-ACTIVITY-DATE     PIC 9(08).
           05  ACCT-DORMANT-DATE           PIC 9(08).
           05  ACCT-ESCHEAT-STATE          PIC X(02).
      *--- BALANCES ---------------------------------------------------*
           05  ACCT-LEDGER-BAL             PIC S9(13)V99 COMP-3.
           05  ACCT-AVAIL-BAL              PIC S9(13)V99 COMP-3.
           05  ACCT-HOLD-AMT               PIC S9(13)V99 COMP-3.
           05  ACCT-MEMO-DR-TODAY          PIC S9(13)V99 COMP-3.
           05  ACCT-MEMO-CR-TODAY          PIC S9(13)V99 COMP-3.
           05  ACCT-PREV-DAY-LEDGER-BAL    PIC S9(13)V99 COMP-3.
      *--- OVERDRAFT -------------------------------------------------*
           05  ACCT-OD-LIMIT               PIC S9(09)V99 COMP-3.
           05  ACCT-OD-OPT-IN              PIC X(01).
               88  ACCT-OD-OPTED-IN                  VALUE 'Y'.
           05  ACCT-OD-PROTECT-ACCT        PIC X(12).
           05  ACCT-OD-COUNT-YTD           PIC 9(03).
           05  ACCT-NSF-COUNT-12M          PIC 9(03).
           05  ACCT-DAYS-OVERDRAWN         PIC 9(03).
           05  ACCT-OD-FEES-TODAY          PIC 9(02).
      *--- INTEREST --------------------------------------------------*
           05  ACCT-INT-RATE               PIC 9(03)V9(06).
           05  ACCT-PROMO-RATE             PIC 9(03)V9(06).
           05  ACCT-PROMO-EXP-DATE         PIC 9(08).
           05  ACCT-ACCRUED-INT            PIC S9(09)V9(06) COMP-3.
           05  ACCT-INT-PAID-YTD           PIC S9(11)V99 COMP-3.
           05  ACCT-INT-PAID-PRIOR-YR      PIC S9(11)V99 COMP-3.
           05  ACCT-WITHHELD-YTD           PIC S9(11)V99 COMP-3.
           05  ACCT-BACKUP-WH-FLAG         PIC X(01).
               88  ACCT-BACKUP-WITHHOLDING           VALUE 'Y'.
           05  ACCT-LAST-ACCRUAL-DATE      PIC 9(08).
           05  ACCT-LAST-CAP-DATE          PIC 9(08).
      *--- TIME DEPOSIT ----------------------------------------------*
           05  ACCT-TERM-MONTHS            PIC 9(03).
           05  ACCT-MATURITY-DATE          PIC 9(08).
           05  ACCT-ORIG-DEPOSIT-AMT       PIC S9(13)V99 COMP-3.
           05  ACCT-RENEWAL-OPTION         PIC X(01).
               88  ACCT-RENEW-AUTO                   VALUE 'A'.
               88  ACCT-RENEW-TO-SAVINGS             VALUE 'M'.
               88  ACCT-RENEW-PAYOUT                 VALUE 'C'.
           05  ACCT-GRACE-END-DATE         PIC 9(08).
           05  ACCT-PAYOUT-ACCT            PIC X(12).
      *--- CYCLE STATISTICS (RESET AT STATEMENT) ---------------------*
           05  ACCT-BAL-ACCUM-CYC          PIC S9(15)V99 COMP-3.
           05  ACCT-DAYS-IN-CYCLE          PIC 9(03).
           05  ACCT-MIN-BAL-CYC            PIC S9(13)V99 COMP-3.
           05  ACCT-REGD-WD-COUNT          PIC 9(03).
           05  ACCT-REGD-VIOLATIONS-12M    PIC 9(02).
           05  ACCT-DR-COUNT-CYC           PIC 9(05).
           05  ACCT-CR-COUNT-CYC           PIC 9(05).
           05  ACCT-FEE-WAIVER-CODE        PIC X(02).
           05  ACCT-FEES-MTD               PIC S9(09)V99 COMP-3.
           05  ACCT-FEES-YTD               PIC S9(09)V99 COMP-3.
           05  ACCT-STMT-CYCLE-DAY         PIC 9(02).
           05  ACCT-STMT-FREQ              PIC X(01).
               88  ACCT-STMT-MONTHLY                 VALUE 'M'.
               88  ACCT-STMT-QUARTERLY               VALUE 'Q'.
           05  ACCT-LAST-STMT-DATE         PIC 9(08).
           05  ACCT-ESTMT-FLAG             PIC X(01).
      *--- DAILY LIMITS / CTR AGGREGATION (RESET AT EOD) -------------*
           05  ACCT-DAILY-WD-LIMIT         PIC S9(09)V99 COMP-3.
           05  ACCT-DAILY-WD-USED          PIC S9(09)V99 COMP-3.
           05  ACCT-DAILY-ACH-LIMIT        PIC S9(13)V99 COMP-3.
           05  ACCT-DAILY-WIRE-LIMIT       PIC S9(13)V99 COMP-3.
           05  ACCT-CASH-IN-TODAY          PIC S9(11)V99 COMP-3.
           05  ACCT-CASH-OUT-TODAY         PIC S9(11)V99 COMP-3.
      *--- TREASURY / CASH MANAGEMENT --------------------------------*
           05  ACCT-PARENT-ACCT            PIC X(12).
           05  ACCT-VIRTUAL-FLAG           PIC X(01).
               88  ACCT-IS-VIRTUAL                   VALUE 'Y'.
           05  ACCT-SWEEP-FLAG             PIC X(01).
               88  ACCT-HAS-SWEEP                    VALUE 'Y'.
           05  ACCT-SWEEP-ID               PIC X(08).
           05  ACCT-POOL-ID                PIC X(08).
           05  ACCT-ANALYSIS-FLAG          PIC X(01).
               88  ACCT-ON-ANALYSIS                  VALUE 'Y'.
      *--- MAINTENANCE -----------------------------------------------*
           05  ACCT-LAST-MAINT-DATE        PIC 9(08).
           05  ACCT-LAST-MAINT-USER        PIC X(08).
           05  FILLER                      PIC X(40).
