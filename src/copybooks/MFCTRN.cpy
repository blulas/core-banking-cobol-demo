      *================================================================*
      * COPYBOOK : MFCTRN                                              *
      * PURPOSE  : MICROFICHE STATEMENTS DAILY TRANSACTION RECORD      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-05-28 BHS  ORIGINAL                                       *
      * 1996-04-20 GAF  PERFORMANCE - POSTAGE READ AHEAD CHG2191       *
      * 1997-06-04 MKL  POSTAGE CUTOFF MOVED REQ9810                   *
      * 1998-07-20 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-11-12 TWH  STORAGE LIMIT REVISED REQ0979                  *
      * 2006-05-14 RJT  RESEARCH RERUN PROTECTION REQ1843              *
      *================================================================*
       01  MFC-TRAN-REC.
           05  MFCT-KEY                    PIC X(12).
           05  MFCT-OPERATOR               PIC X(08).
           05  MFCT-REJECT-CD              PIC X(04).
           05  MFCT-MEMO                   PIC X(30).
           05  MFC-TRAN-AMOUNTS.
               10  MFCT-STORAGE-AMT        PIC S9(13)V99 COMP-3.
               10  MFCT-RESEARCH-AMT       PIC S9(13)V99 COMP-3.
               10  MFCT-POSTAGE-AMT        PIC S9(13)V99 COMP-3.
               10  MFCT-COPY-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  MFC-TRAN-DATES.
               10  MFCT-EFF-DATE           PIC 9(08).
               10  MFCT-POST-DATE          PIC 9(08).
           05  MFC-TRAN-CODES.
               10  MFCT-TRAN-CD            PIC X(02).
                   88  MFCT-TRAN-CD-IX             VALUE 'IX'.
                   88  MFCT-TRAN-CD-RS             VALUE 'RS'.
                   88  MFCT-TRAN-CD-CP             VALUE 'CP'.
                   88  MFCT-TRAN-CD-PG             VALUE 'PG'.
               10  MFCT-CHANNEL-CD         PIC X(03).
                   88  MFCT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  MFCT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  MFCT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  MFCT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  MFCT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  MFCT-REVERSAL-FLAG      PIC X(01).
                   88  MFCT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  MFC-TRAN-COUNTERS.
               10  MFCT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
