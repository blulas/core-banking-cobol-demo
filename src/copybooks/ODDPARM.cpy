      *================================================================*
      * COPYBOOK : ODDPARM                                             *
      * PURPOSE  : LINKAGE FOR ODDECIDE - OVERDRAFT PAY / RETURN       *
      *            DECISION FOR ONE DEBIT ITEM.                        *
      *----------------------------------------------------------------*
      * DECISION  P = PAY            (RC 00 - FUNDS AVAILABLE, OR      *
      *                               RC 04 - PAID INTO OVERDRAFT)     *
      *           X = PAY WITH OD PROTECTION TRANSFER  (RC 04)         *
      *           R = RETURN / DECLINE                 (RC 08)         *
      * REASON CODES (ODD-REASON-CODE)                                 *
      *   FUND  SUFFICIENT AVAILABLE FUNDS                             *
      *   XFER  COVERED BY OD PROTECTION TRANSFER                      *
      *   ODPD  PAID INTO OVERDRAFT AT BANK DISCRETION                 *
      *   FRCD  BANK-ORIGINATED DEBIT - FORCE POSTED                   *
      *   POOL  CORPORATE POOLED ACCOUNT - COVERED BY POOL             *
      *   ZBAF  ZERO BALANCE SUB-ACCOUNT - FUNDED BY PARENT SWEEP      *
      *   NOOD  PRODUCT DOES NOT ALLOW OVERDRAFT                       *
      *   REGE  ATM / ONE-TIME DEBIT CARD - NOT OPTED IN (NO FEE)      *
      *   WIRE  OUTGOING WIRE - NEVER PAID INTO OVERDRAFT              *
      *   NEWA  ACCOUNT OPEN LESS THAN 30 DAYS                         *
      *   ODAG  ACCOUNT OVERDRAWN MORE THAN 30 DAYS                    *
      *   NSFX  6 OR MORE NSF ITEMS IN 12 MONTHS                       *
      *   OLIM  ITEM WOULD EXCEED THE OVERDRAFT LIMIT                  *
      * FEE WAIVE CODES (ODD-FEE-WAIVE-CODE)                           *
      *   PWWV  PRIVATE WEALTH SEGMENT                                 *
      *   FCAP  DAILY MAXIMUM OF 3 OD/NSF FEES REACHED                 *
      *   DMIN  ITEM $5.00 OR LESS                                     *
      *   DMOD  RESULTING OVERDRAFT $5.00 OR LESS                      *
      *   EMPL  EMPLOYEE - 50 PERCENT DISCOUNT (FEE STILL CHARGED)     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-11 RJT  ORIGINAL                                       *
      * 2003-08-04 BHS  OD PROTECTION TRANSFER FROM LINKED SAVINGS     *
      * 2009-11-17 DPS  REG E OPT-IN (REG-2010-E)                      *
      * 2010-07-01 DPS  SEPARATE OD / NSF FEE CODES                    *
      * 2017-04-03 AKW  DE MINIMIS FEE WAIVERS (CHG1894)               *
      * 2019-03-25 AKW  POOLED CORPORATE ACCOUNTS                      *
      * 2019-11-04 AKW  ZBA SUB-ACCOUNTS (INC25120)                    *
      *================================================================*
       01  ODDECIDE-PARMS.
           05  ODD-INPUT.
               10  ODD-TRAN-CODE           PIC X(04).
               10  ODD-CHANNEL             PIC X(03).
               10  ODD-AMOUNT              PIC S9(13)V99 COMP-3.
               10  ODD-AVAIL-BAL           PIC S9(13)V99 COMP-3.
               10  ODD-OD-LIMIT            PIC S9(09)V99 COMP-3.
               10  ODD-OD-OPT-IN           PIC X(01).
               10  ODD-PROD-OD-ALLOWED     PIC X(01).
      *        Y = NOTIONAL POOL MEMBER, Z = ZBA SUB-ACCOUNT, N = NO
               10  ODD-POOL-FLAG           PIC X(01).
               10  ODD-NSF-COUNT-12M       PIC 9(03).
               10  ODD-DAYS-OVERDRAWN      PIC 9(03).
               10  ODD-ACCT-OPEN-DATE      PIC 9(08).
               10  ODD-BUSINESS-DATE       PIC 9(08).
               10  ODD-PROTECT-FLAG        PIC X(01).
                   88  ODD-HAS-PROTECTION            VALUE 'Y'.
               10  ODD-PROTECT-AVAIL-BAL   PIC S9(13)V99 COMP-3.
               10  ODD-CUST-SEGMENT        PIC X(02).
               10  ODD-EMPLOYEE-FLAG       PIC X(01).
      *    OD/NSF FEES ALREADY ASSESSED TODAY - UPDATED ON RETURN
           05  ODD-FEES-TODAY              PIC 9(02).
           05  ODD-OUTPUT.
               10  ODD-DECISION            PIC X(01).
                   88  ODD-PAY                       VALUE 'P'.
                   88  ODD-PAY-WITH-XFER             VALUE 'X'.
                   88  ODD-RETURN                    VALUE 'R'.
               10  ODD-PAID-INTO-OD        PIC X(01).
                   88  ODD-INTO-OVERDRAFT            VALUE 'Y'.
               10  ODD-REASON-CODE         PIC X(04).
               10  ODD-XFER-AMOUNT         PIC S9(13)V99 COMP-3.
               10  ODD-XFER-FEE            PIC S9(07)V99 COMP-3.
               10  ODD-FEE-CODE            PIC X(04).
               10  ODD-FEE-AMOUNT          PIC S9(07)V99 COMP-3.
               10  ODD-FEE-WAIVE-CODE      PIC X(04).
      *        AVAILABLE BALANCE AFTER XFER, ITEM (IF PAID) AND FEES
               10  ODD-RESULT-AVAIL        PIC S9(13)V99 COMP-3.
           05  ODD-RETURN-CODE             PIC 9(02).
