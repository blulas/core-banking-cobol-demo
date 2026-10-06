      *================================================================*
      * COPYBOOK : TBLTRN                                              *
      * PURPOSE  : TELLER BALANCING DAILY TRANSACTION RECORD           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-02-02 BHS  ORIGINAL                                       *
      * 1995-07-05 GAF  VAULT-SELL TABLE EXPANDED PRB6858              *
      * 1997-01-26 BHS  NEW VAULT-BUY REPORT COLUMNS CHG4726           *
      * 1999-03-13 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-08-26 LMB  CASH-OUT RERUN PROTECTION CHG3336              *
      * 2002-04-15 GAF  VAULT-BUY EXCEPTIONS TO OPS QUEUE PRB5948      *
      * 2002-07-18 DPS  CASH-OUT RERUN PROTECTION CHG8461              *
      * 2003-02-03 SKN  VAULT-BUY NOTICE TEXT PER LEGAL REQ6606        *
      * 2005-10-08 PQR  NEW VAULT-BUY REPORT COLUMNS CHG9716           *
      * 2006-07-22 LMB  OVERSHORT STATUS CODES ADDED PRB7117           *
      * 2006-12-11 JLM  CASH-OUT CALCULATION CORRECTED REQ0914         *
      * 2007-02-03 DPS  AUDIT FINDING - BATCH CONTROL TOTALS PRB1945   *
      *================================================================*
       01  TBL-TRAN-REC.
           05  TBLT-KEY                    PIC X(12).
           05  TBLT-OPERATOR               PIC X(08).
           05  TBLT-REJECT-CD              PIC X(04).
           05  TBLT-MEMO                   PIC X(30).
           05  TBL-TRAN-AMOUNTS.
               10  TBLT-CASH-OUT-AMT       PIC S9(13)V99 COMP-3.
               10  TBLT-BATCH-AMT          PIC S9(13)V99 COMP-3.
               10  TBLT-VAULT-SELL-AMT     PIC S9(13)V99 COMP-3.
               10  TBLT-OVERSHORT-AMT      PIC S9(13)V99 COMP-3.
           05  TBL-TRAN-DATES.
               10  TBLT-EFF-DATE           PIC 9(08).
               10  TBLT-POST-DATE          PIC 9(08).
           05  TBL-TRAN-CODES.
               10  TBLT-TRAN-CD            PIC X(02).
                   88  TBLT-TRAN-CD-BI             VALUE 'BI'.
                   88  TBLT-TRAN-CD-BO             VALUE 'BO'.
                   88  TBLT-TRAN-CD-VB             VALUE 'VB'.
                   88  TBLT-TRAN-CD-VS             VALUE 'VS'.
                   88  TBLT-TRAN-CD-OS             VALUE 'OS'.
               10  TBLT-CHANNEL-CD         PIC X(03).
                   88  TBLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  TBLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  TBLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  TBLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  TBLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  TBLT-REVERSAL-FLAG      PIC X(01).
                   88  TBLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  TBL-TRAN-COUNTERS.
               10  TBLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
