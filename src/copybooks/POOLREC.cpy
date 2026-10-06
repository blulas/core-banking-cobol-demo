      *================================================================*
      * COPYBOOK : POOLREC                                             *
      * PURPOSE  : NOTIONAL CASH POOL DEFINITION                       *
      * FILE     : POOLMST (KSDS, KEY = POOL-ID)                       *
      *   BALANCES ARE NOT MOVED. INTEREST IS CALCULATED ON THE NET    *
      *   POOL POSITION AND ALLOCATED TO MEMBERS (SEE POOLALOC).       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-03-15 BHS  ORIGINAL - NOTIONAL POOLING                    *
      * 2009-01-20 DPS  RATES AS SPREADS TO FED FUNDS / PRIME          *
      * 2013-06-11 DPS  POOL LIMIT (AGGREGATE DEBIT) - CREDIT RISK     *
      * 2019-03-25 AKW  MEMBER TABLE EXPANDED 10 -> 20                 *
      *================================================================*
       01  POOL-RECORD.
           05  POOL-ID                     PIC X(08).
           05  POOL-NAME                   PIC X(30).
           05  POOL-CUST-ID                PIC X(10).
           05  POOL-HEADER-ACCT            PIC X(12).
           05  POOL-MEMBER-COUNT           PIC 9(02).
           05  POOL-MEMBER-TABLE.
               10  POOL-MEMBER-ACCT        PIC X(12) OCCURS 20 TIMES.
           05  POOL-ALLOC-METHOD           PIC X(01).
      *        P = PROPORTIONAL TO EACH MEMBER'S CONTRIBUTION
      *        H = ALL INTEREST TO THE HEADER ACCOUNT
               88  POOL-ALLOC-PROPORTIONAL           VALUE 'P'.
               88  POOL-ALLOC-HEADER                 VALUE 'H'.
      *    CREDIT RATE = FED FUNDS - CREDIT SPREAD (FLOOR 0)
      *    DEBIT RATE  = PRIME     + DEBIT SPREAD
           05  POOL-CREDIT-SPREAD          PIC 9(03)V9(06).
           05  POOL-DEBIT-SPREAD           PIC 9(03)V9(06).
      *    MAXIMUM AGGREGATE DEBIT (SUM OF OVERDRAWN MEMBERS)
           05  POOL-LIMIT                  PIC S9(13)V99 COMP-3.
           05  POOL-STATUS                 PIC X(01).
               88  POOL-ACTIVE                       VALUE 'A'.
               88  POOL-SUSPENDED                    VALUE 'S'.
               88  POOL-CLOSED                       VALUE 'C'.
           05  POOL-EFF-DATE               PIC 9(08).
           05  POOL-LAST-ALLOC-DATE        PIC 9(08).
           05  POOL-LAST-CREDIT-INT        PIC S9(11)V99 COMP-3.
           05  POOL-LAST-DEBIT-INT         PIC S9(11)V99 COMP-3.
           05  POOL-LAST-NET-POSITION      PIC S9(13)V99 COMP-3.
           05  FILLER                      PIC X(30).
