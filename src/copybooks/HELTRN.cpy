      *================================================================*
      * COPYBOOK : HELTRN                                              *
      * PURPOSE  : HOME EQUITY LINES DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-24 RJT  ORIGINAL                                       *
      * 1995-01-16 RJT  PERFORMANCE - MIN-PAY READ AHEAD CHG7461       *
      * 2000-04-11 MKL  INTEREST CALCULATION CORRECTED CHG5595         *
      * 2000-10-05 EWS  APPRAISE CUTOFF MOVED INC7282                  *
      * 2003-09-23 TWH  PERFORMANCE - INTEREST READ AHEAD INC9156      *
      * 2005-06-23 RJT  APPRAISE RERUN PROTECTION INC5910              *
      * 2006-12-17 BHS  INTEREST ROUNDING - HALF UP REQ0353            *
      * 2007-08-24 PQR  AUDIT FINDING - INTEREST CONTROL TOTALS CHG1327*
      * 2008-12-17 SKN  PERFORMANCE - MIN-PAY READ AHEAD REQ8611       *
      *================================================================*
       01  HEL-TRAN-REC.
           05  HELT-KEY                    PIC X(12).
           05  HELT-OPERATOR               PIC X(08).
           05  HELT-REJECT-CD              PIC X(04).
           05  HELT-MEMO                   PIC X(30).
           05  HEL-TRAN-AMOUNTS.
               10  HELT-DRAW-AMT           PIC S9(13)V99 COMP-3.
               10  HELT-APPRAISE-AMT       PIC S9(13)V99 COMP-3.
               10  HELT-ANNUAL-FEE-AMT     PIC S9(13)V99 COMP-3.
               10  HELT-LIMIT-AMT          PIC S9(13)V99 COMP-3.
           05  HEL-TRAN-DATES.
               10  HELT-EFF-DATE           PIC 9(08).
               10  HELT-POST-DATE          PIC 9(08).
           05  HEL-TRAN-CODES.
               10  HELT-TRAN-CD            PIC X(02).
                   88  HELT-TRAN-CD-DR             VALUE 'DR'.
                   88  HELT-TRAN-CD-PY             VALUE 'PY'.
                   88  HELT-TRAN-CD-FZ             VALUE 'FZ'.
                   88  HELT-TRAN-CD-RC             VALUE 'RC'.
                   88  HELT-TRAN-CD-AF             VALUE 'AF'.
               10  HELT-CHANNEL-CD         PIC X(03).
                   88  HELT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  HELT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  HELT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  HELT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  HELT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  HELT-REVERSAL-FLAG      PIC X(01).
                   88  HELT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  HEL-TRAN-COUNTERS.
               10  HELT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
