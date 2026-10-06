      *================================================================*
      * COPYBOOK : BCDTRN                                              *
      * PURPOSE  : BUMP-UP CERTIFICATES DAILY TRANSACTION RECORD       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-17 JLM  ORIGINAL                                       *
      * 1993-03-15 LMB  BUMP ROUNDING - HALF UP CHG4894                *
      * 1994-07-14 DPS  PENALTY CUTOFF MOVED CHG0757                   *
      * 1995-02-16 MKL  INTEREST ROUNDING - HALF UP CHG1590            *
      * 1997-10-05 JLM  ADDON NOTICE TEXT PER LEGAL INC1765            *
      * 1999-09-08 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-15 BHS  INTEREST NOTICE TEXT PER LEGAL INC9745         *
      * 2000-04-02 TWH  NEW PENALTY REPORT COLUMNS CHG3597             *
      * 2000-02-21 DPS  INTEREST RERUN PROTECTION REQ2519              *
      * 2005-02-09 MKL  ADDON FEE SCHEDULE UPDATE PRB6156              *
      *================================================================*
       01  BCD-TRAN-REC.
           05  BCDT-KEY                    PIC X(12).
           05  BCDT-OPERATOR               PIC X(08).
           05  BCDT-REJECT-CD              PIC X(04).
           05  BCDT-MEMO                   PIC X(30).
           05  BCD-TRAN-AMOUNTS.
               10  BCDT-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  BCDT-BUMP-AMT           PIC S9(13)V99 COMP-3.
               10  BCDT-PENALTY-AMT        PIC S9(13)V99 COMP-3.
               10  BCDT-ADDON-AMT          PIC S9(13)V99 COMP-3.
           05  BCD-TRAN-DATES.
               10  BCDT-EFF-DATE           PIC 9(08).
               10  BCDT-POST-DATE          PIC 9(08).
           05  BCD-TRAN-CODES.
               10  BCDT-TRAN-CD            PIC X(02).
                   88  BCDT-TRAN-CD-BU             VALUE 'BU'.
                   88  BCDT-TRAN-CD-AD             VALUE 'AD'.
                   88  BCDT-TRAN-CD-WD             VALUE 'WD'.
                   88  BCDT-TRAN-CD-MT             VALUE 'MT'.
               10  BCDT-CHANNEL-CD         PIC X(03).
                   88  BCDT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  BCDT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  BCDT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  BCDT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  BCDT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  BCDT-REVERSAL-FLAG      PIC X(01).
                   88  BCDT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  BCD-TRAN-COUNTERS.
               10  BCDT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
