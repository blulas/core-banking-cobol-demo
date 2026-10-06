      *================================================================*
      * COPYBOOK : TRSTRN                                              *
      * PURPOSE  : PERSONAL TRUST DAILY TRANSACTION RECORD             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-12-04 EWS  ORIGINAL                                       *
      * 1990-11-11 SKN  TRUSTEE-FEE STATUS CODES ADDED PRB7507         *
      * 1992-07-24 BHS  DISTRIB ROUNDING - HALF UP CHG6369             *
      * 1992-08-12 GAF  PERFORMANCE - DISTRIB READ AHEAD REQ6548       *
      * 2000-03-05 LMB  AUDIT FINDING - DISTRIB CONTROL TOTALS REQ8856 *
      * 2001-04-02 DPS  INCOME EXCEPTIONS TO OPS QUEUE CHG9703         *
      * 2003-03-19 EWS  INCOME LIMIT REVISED INC0349                   *
      * 2005-02-25 JLM  NEW INCOME REPORT COLUMNS PRB5080              *
      * 2005-06-27 GAF  NEW TRUSTEE-FEE REPORT COLUMNS INC2421         *
      *================================================================*
       01  TRS-TRAN-REC.
           05  TRST-KEY                    PIC X(12).
           05  TRST-OPERATOR               PIC X(08).
           05  TRST-REJECT-CD              PIC X(04).
           05  TRST-MEMO                   PIC X(30).
           05  TRS-TRAN-AMOUNTS.
               10  TRST-INCOME-AMT         PIC S9(13)V99 COMP-3.
               10  TRST-PRINCIPAL-AMT      PIC S9(13)V99 COMP-3.
               10  TRST-TRUSTEE-FEE-AMT    PIC S9(13)V99 COMP-3.
               10  TRST-DISTRIB-AMT        PIC S9(13)V99 COMP-3.
           05  TRS-TRAN-DATES.
               10  TRST-EFF-DATE           PIC 9(08).
               10  TRST-POST-DATE          PIC 9(08).
           05  TRS-TRAN-CODES.
               10  TRST-TRAN-CD            PIC X(02).
                   88  TRST-TRAN-CD-IN             VALUE 'IN'.
                   88  TRST-TRAN-CD-DS             VALUE 'DS'.
                   88  TRST-TRAN-CD-FE             VALUE 'FE'.
                   88  TRST-TRAN-CD-RB             VALUE 'RB'.
                   88  TRST-TRAN-CD-TX             VALUE 'TX'.
               10  TRST-CHANNEL-CD         PIC X(03).
                   88  TRST-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  TRST-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  TRST-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  TRST-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  TRST-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  TRST-REVERSAL-FLAG      PIC X(01).
                   88  TRST-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  TRS-TRAN-COUNTERS.
               10  TRST-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
