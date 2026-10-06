      *================================================================*
      * COPYBOOK : STLTRN                                              *
      * PURPOSE  : STUDENT LOANS DAILY TRANSACTION RECORD              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-07-13 BHS  ORIGINAL                                       *
      * 1989-03-14 SKN  PRINCIPAL ROUNDING - HALF UP PRB3698           *
      * 1999-11-25 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-13 GAF  DEFERINT EXCEPTIONS TO OPS QUEUE REQ9894       *
      * 2003-07-02 MKL  PERFORMANCE - CAPINT READ AHEAD PRB1791        *
      *================================================================*
       01  STL-TRAN-REC.
           05  STLT-KEY                    PIC X(12).
           05  STLT-OPERATOR               PIC X(08).
           05  STLT-REJECT-CD              PIC X(04).
           05  STLT-MEMO                   PIC X(30).
           05  STL-TRAN-AMOUNTS.
               10  STLT-DISBURSE-AMT       PIC S9(13)V99 COMP-3.
               10  STLT-PRINCIPAL-AMT      PIC S9(13)V99 COMP-3.
               10  STLT-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  STLT-CAPINT-AMT         PIC S9(13)V99 COMP-3.
           05  STL-TRAN-DATES.
               10  STLT-EFF-DATE           PIC 9(08).
               10  STLT-POST-DATE          PIC 9(08).
           05  STL-TRAN-CODES.
               10  STLT-TRAN-CD            PIC X(02).
                   88  STLT-TRAN-CD-DB             VALUE 'DB'.
                   88  STLT-TRAN-CD-PY             VALUE 'PY'.
                   88  STLT-TRAN-CD-CP             VALUE 'CP'.
                   88  STLT-TRAN-CD-DF             VALUE 'DF'.
                   88  STLT-TRAN-CD-FB             VALUE 'FB'.
               10  STLT-CHANNEL-CD         PIC X(03).
                   88  STLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  STLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  STLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  STLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  STLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  STLT-REVERSAL-FLAG      PIC X(01).
                   88  STLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  STL-TRAN-COUNTERS.
               10  STLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
