      *================================================================*
      * COPYBOOK : TRVPARM                                             *
      * PURPOSE  : LINKAGE FOR TRANVAL - MONETARY TRANSACTION EDIT.    *
      *            CALLER LOOKS UP THE ACCOUNT AND PRODUCT AND PASSES  *
      *            THE FIELDS BELOW. TRANVAL DOES NO FILE I/O.         *
      *----------------------------------------------------------------*
      * REJECT REASON CODES (TRV-REJECT-CODE)                          *
      *   TCIN  TRANSACTION CODE NOT IN TRANCODE TABLE                 *
      *   AMT0  AMOUNT NOT GREATER THAN ZERO                           *
      *   ACNF  ACCOUNT NOT ON FILE                                    *
      *   PRNF  PRODUCT NOT ON FILE                                    *
      *   EDIV  EFFECTIVE DATE INVALID                                 *
      *   EDFT  EFFECTIVE DATE > 5 BUSINESS DAYS IN THE FUTURE         *
      *   EDBK  EFFECTIVE DATE > 30 CALENDAR DAYS BACK (EXCEPT ADJ*)   *
      *   CHNL  TRAN CODE NOT PERMITTED ON THIS CHANNEL                *
      *   ACLS  ACCOUNT CLOSED                                         *
      *   AFRZ  ACCOUNT FROZEN - ONLY DREV ACCEPTED                    *
      *   ARST  ACCOUNT RESTRICTED - CUSTOMER DEBITS NOT ACCEPTED      *
      *   ADRM  ACCOUNT DORMANT - DEBIT NEEDS BRANCH + ID VERIFICATION *
      *   AESC  ACCOUNT ESCHEATED TO STATE                             *
      *   ACOF  ACCOUNT CHARGED OFF - CREDITS ONLY                     *
      *   APND  ACCOUNT PENDING OPEN - CREDITS ONLY                    *
      *   TDDR  TIME DEPOSIT - DEBIT CODE NOT PERMITTED                *
      *   TDCR  TIME DEPOSIT - ADDITIONAL DEPOSIT OUTSIDE GRACE PERIOD *
      *   NCHK  PRODUCT DOES NOT PERMIT CHECKS                         *
      *   DWLM  DAILY ATM / CASH WITHDRAWAL LIMIT EXCEEDED             *
      *   (NSF  IS SET BY POSTTRAN, NOT BY TRANVAL)                    *
      *----------------------------------------------------------------*
      * RETURN CODE: 00 ACCEPTED, 08 REJECTED, 12 BAD PARAMETERS       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-05-14 BHS  CHANNEL COMPATIBILITY, DAILY WD LIMIT          *
      * 2014-01-06 DPS  ESCHEATED / CHARGED-OFF STATES                 *
      * 2018-02-20 AKW  CD GRACE PERIOD DEPOSITS (CHG2210)             *
      *================================================================*
       01  TRANVAL-PARMS.
           05  TRV-INPUT.
               10  TRV-TRAN-CODE           PIC X(04).
               10  TRV-AMOUNT              PIC S9(13)V99 COMP-3.
               10  TRV-CHANNEL             PIC X(03).
               10  TRV-EFFECTIVE-DATE      PIC 9(08).
               10  TRV-BUSINESS-DATE       PIC 9(08).
               10  TRV-ACCT-FOUND-FLAG     PIC X(01).
                   88  TRV-ACCT-FOUND                VALUE 'Y'.
               10  TRV-ACCT-STATUS         PIC X(01).
               10  TRV-ACCT-CATEGORY       PIC X(02).
               10  TRV-MATURITY-DATE       PIC 9(08).
               10  TRV-GRACE-END-DATE      PIC 9(08).
               10  TRV-PROD-FOUND-FLAG     PIC X(01).
                   88  TRV-PROD-FOUND                VALUE 'Y'.
               10  TRV-PROD-CHECK-ALLOWED  PIC X(01).
               10  TRV-DAILY-WD-LIMIT      PIC S9(09)V99 COMP-3.
               10  TRV-DAILY-WD-USED       PIC S9(09)V99 COMP-3.
      *        Y = CUSTOMER IDENTITY VERIFIED IN PERSON (BRANCH)
               10  TRV-ID-VERIFIED-FLAG    PIC X(01).
           05  TRV-OUTPUT.
      *        ATTRIBUTES OF THE TRAN CODE (FROM TRANCODE TABLE)
               10  TRV-DR-CR               PIC X(01).
               10  TRV-CASH-FLAG           PIC X(01).
               10  TRV-REGD-FLAG           PIC X(01).
               10  TRV-CUST-INIT-FLAG      PIC X(01).
               10  TRV-POST-PRIORITY       PIC 9(01).
      *        Y = POSTING THIS ITEM REACTIVATES A DORMANT ACCOUNT
               10  TRV-REACTIVATE-FLAG     PIC X(01).
      *        Y = AMOUNT COUNTS AGAINST ACCT-DAILY-WD-LIMIT
               10  TRV-WD-LIMIT-FLAG       PIC X(01).
               10  TRV-REJECT-CODE         PIC X(04).
               10  TRV-REJECT-TEXT         PIC X(40).
           05  TRV-RETURN-CODE             PIC 9(02).
               88  TRV-ACCEPTED                      VALUE 00.
               88  TRV-REJECTED                      VALUE 08.
               88  TRV-PARM-ERROR                    VALUE 12.
