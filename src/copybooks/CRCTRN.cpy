      *================================================================*
      * COPYBOOK : CRCTRN                                              *
      * PURPOSE  : PROPRIETARY CREDIT CARD DAILY TRANSACTION RECORD    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-16 TWH  ORIGINAL                                       *
      * 1995-10-28 JLM  FINANCE ROUNDING - HALF UP REQ9312             *
      * 1999-05-27 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-21 BHS  PERFORMANCE - PAYMENT READ AHEAD REQ5526       *
      * 2000-08-08 SKN  NEW PURCHASE REPORT COLUMNS INC2423            *
      * 2003-10-11 CRP  FINANCE ROUNDING - HALF UP INC7979             *
      * 2006-11-26 RJT  NEW LATE-FEE REPORT COLUMNS PRB7800            *
      * 2007-06-17 LMB  LATE-FEE RERUN PROTECTION REQ2597              *
      *================================================================*
       01  CRC-TRAN-REC.
           05  CRCT-KEY                    PIC X(12).
           05  CRCT-OPERATOR               PIC X(08).
           05  CRCT-REJECT-CD              PIC X(04).
           05  CRCT-MEMO                   PIC X(30).
           05  CRC-TRAN-AMOUNTS.
               10  CRCT-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  CRCT-CASH-ADV-AMT       PIC S9(13)V99 COMP-3.
               10  CRCT-MIN-DUE-AMT        PIC S9(13)V99 COMP-3.
               10  CRCT-LIMIT-AMT          PIC S9(13)V99 COMP-3.
           05  CRC-TRAN-DATES.
               10  CRCT-EFF-DATE           PIC 9(08).
               10  CRCT-POST-DATE          PIC 9(08).
           05  CRC-TRAN-CODES.
               10  CRCT-TRAN-CD            PIC X(02).
                   88  CRCT-TRAN-CD-PU             VALUE 'PU'.
                   88  CRCT-TRAN-CD-CA             VALUE 'CA'.
                   88  CRCT-TRAN-CD-PY             VALUE 'PY'.
                   88  CRCT-TRAN-CD-FC             VALUE 'FC'.
                   88  CRCT-TRAN-CD-LF             VALUE 'LF'.
               10  CRCT-CHANNEL-CD         PIC X(03).
                   88  CRCT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  CRCT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  CRCT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  CRCT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  CRCT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  CRCT-REVERSAL-FLAG      PIC X(01).
                   88  CRCT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  CRC-TRAN-COUNTERS.
               10  CRCT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
