      *================================================================*
      * COPYBOOK : SBNTRN                                              *
      * PURPOSE  : SAVINGS BOND SERVICES DAILY TRANSACTION RECORD      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-14 CRP  ORIGINAL                                       *
      * 1997-12-01 SKN  ACCRUAL STATUS CODES ADDED REQ4632             *
      * 2001-08-19 PQR  INTEREST TABLE EXPANDED REQ4542                *
      * 2003-02-02 MKL  TAXABLE LIMIT REVISED INC6675                  *
      * 2004-02-14 RJT  FACE LIMIT REVISED CHG9135                     *
      * 2006-11-22 TWH  NEW TAXABLE REPORT COLUMNS REQ5560             *
      * 2008-02-28 EWS  TAXABLE CUTOFF MOVED REQ4851                   *
      * 2009-02-10 TWH  REDEEM NOTICE TEXT PER LEGAL INC6444           *
      *================================================================*
       01  SBN-TRAN-REC.
           05  SBNT-KEY                    PIC X(12).
           05  SBNT-OPERATOR               PIC X(08).
           05  SBNT-REJECT-CD              PIC X(04).
           05  SBNT-MEMO                   PIC X(30).
           05  SBN-TRAN-AMOUNTS.
               10  SBNT-REDEEM-AMT         PIC S9(13)V99 COMP-3.
               10  SBNT-ACCRUAL-AMT        PIC S9(13)V99 COMP-3.
               10  SBNT-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  SBNT-TAXABLE-AMT        PIC S9(13)V99 COMP-3.
           05  SBN-TRAN-DATES.
               10  SBNT-EFF-DATE           PIC 9(08).
               10  SBNT-POST-DATE          PIC 9(08).
           05  SBN-TRAN-CODES.
               10  SBNT-TRAN-CD            PIC X(02).
                   88  SBNT-TRAN-CD-BY             VALUE 'BY'.
                   88  SBNT-TRAN-CD-RD             VALUE 'RD'.
                   88  SBNT-TRAN-CD-IR             VALUE 'IR'.
                   88  SBNT-TRAN-CD-TX             VALUE 'TX'.
                   88  SBNT-TRAN-CD-AD             VALUE 'AD'.
               10  SBNT-CHANNEL-CD         PIC X(03).
                   88  SBNT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  SBNT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  SBNT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  SBNT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  SBNT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  SBNT-REVERSAL-FLAG      PIC X(01).
                   88  SBNT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  SBN-TRAN-COUNTERS.
               10  SBNT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
