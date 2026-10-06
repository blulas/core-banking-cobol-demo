      *================================================================*
      * COPYBOOK : GLCHART                                             *
      * PURPOSE  : CHART OF ACCOUNTS (HARDCODED). ENTRY LAYOUT:        *
      *   GL ACCOUNT(10) DESCRIPTION(30) TYPE(1) NORMAL BALANCE(1)     *
      *   TYPE A ASSET, L LIABILITY, Q EQUITY, I INCOME, E EXPENSE     *
      *   ANY CHANGE MUST BE AGREED WITH FINANCE / ORACLE GL MAPPING.  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2001-10-01 BHS  ACH / FEDWIRE SETTLEMENT SPLIT 1150 / 1160     *
      * 2010-07-01 DPS  REG E - FEE INCOME SPLIT BY FEE TYPE 4110-4190 *
      * 2011-04-12 DPS  DDA SPLIT CONSUMER 2010 / CORPORATE 2020       *
      * 2013-06-11 DPS  4020 DEBIT INTEREST ON NOTIONAL POOLS          *
      * 2014-01-06 DPS  2310 ESCHEAT PAYABLE                           *
      *================================================================*
       01  GL-CHART-VALUES.
           05  FILLER PIC X(42) VALUE
               '1010      CASH AND DUE FROM FRB         AD'.
           05  FILLER PIC X(42) VALUE
               '1050      CASH ITEMS IN PROCESS         AD'.
           05  FILLER PIC X(42) VALUE
               '1150      ACH SETTLEMENT - FRB          AD'.
           05  FILLER PIC X(42) VALUE
               '1160      FEDWIRE SETTLEMENT - FRB      AD'.
           05  FILLER PIC X(42) VALUE
               '1170      CARD NETWORK SETTLEMENT       AD'.
           05  FILLER PIC X(42) VALUE
               '1190      INTERNAL TRANSFER CLEARING    AD'.
           05  FILLER PIC X(42) VALUE
               '1400      LOANS AND LEASES              AD'.
           05  FILLER PIC X(42) VALUE
               '1420      OVERDRAFT RECEIVABLE          AD'.
           05  FILLER PIC X(42) VALUE
               '1450      ACCRUED INTEREST RECEIVABLE   AD'.
           05  FILLER PIC X(42) VALUE
               '2010      DDA - CONSUMER                LC'.
           05  FILLER PIC X(42) VALUE
               '2020      DDA - CORPORATE               LC'.
           05  FILLER PIC X(42) VALUE
               '2110      SAVINGS DEPOSITS              LC'.
           05  FILLER PIC X(42) VALUE
               '2120      MONEY MARKET DEPOSITS         LC'.
           05  FILLER PIC X(42) VALUE
               '2130      TIME DEPOSITS                 LC'.
           05  FILLER PIC X(42) VALUE
               '2210      ACCRUED INTEREST PAYABLE      LC'.
           05  FILLER PIC X(42) VALUE
               '2310      ESCHEAT PAYABLE               LC'.
           05  FILLER PIC X(42) VALUE
               '2320      BACKUP WITHHOLDING PAYABLE    LC'.
           05  FILLER PIC X(42) VALUE
               '2999      SUSPENSE - UNMAPPED ITEMS     LC'.
           05  FILLER PIC X(42) VALUE
               '3100      SHAREHOLDERS EQUITY           QC'.
           05  FILLER PIC X(42) VALUE
               '4010      LOAN INTEREST INCOME          IC'.
           05  FILLER PIC X(42) VALUE
               '4020      DEBIT INTEREST - NOTIONAL POOLIC'.
           05  FILLER PIC X(42) VALUE
               '4110      FEE INCOME - OVERDRAFT        IC'.
           05  FILLER PIC X(42) VALUE
               '4120      FEE INCOME - NSF              IC'.
           05  FILLER PIC X(42) VALUE
               '4130      FEE INCOME - MAINTENANCE      IC'.
           05  FILLER PIC X(42) VALUE
               '4140      FEE INCOME - WIRE TRANSFER    IC'.
           05  FILLER PIC X(42) VALUE
               '4150      FEE INCOME - ACCOUNT ANALYSIS IC'.
           05  FILLER PIC X(42) VALUE
               '4160      FEE INCOME - REG D EXCESS WD  IC'.
           05  FILLER PIC X(42) VALUE
               '4170      FEE INCOME - LATE PAYMENT     IC'.
           05  FILLER PIC X(42) VALUE
               '4175      CD EARLY WITHDRAWAL PENALTY   IC'.
           05  FILLER PIC X(42) VALUE
               '4180      FEE INCOME - OTHER SERVICE    IC'.
           05  FILLER PIC X(42) VALUE
               '4190      FEE REFUNDS AND WAIVERS       IC'.
           05  FILLER PIC X(42) VALUE
               '5010      INT EXPENSE - CHECKING        ED'.
           05  FILLER PIC X(42) VALUE
               '5020      INT EXPENSE - SAVINGS         ED'.
           05  FILLER PIC X(42) VALUE
               '5030      INT EXPENSE - MONEY MARKET    ED'.
           05  FILLER PIC X(42) VALUE
               '5040      INT EXPENSE - TIME DEPOSITS   ED'.
           05  FILLER PIC X(42) VALUE
               '5050      INT EXPENSE - COMMERCIAL      ED'.
           05  FILLER PIC X(42) VALUE
               '5510      OVERDRAFT CHARGE-OFF LOSS     ED'.
       01  GL-CHART-TABLE REDEFINES GL-CHART-VALUES.
           05  GLC-ENTRY OCCURS 37 TIMES INDEXED BY GLC-IDX.
               10  GLC-GL-ACCOUNT          PIC X(10).
               10  GLC-DESCRIPTION         PIC X(30).
               10  GLC-ACCT-TYPE           PIC X(01).
               10  GLC-NORMAL-BAL          PIC X(01).
       01  GL-CHART-COUNT                  PIC 9(03) VALUE 37.
