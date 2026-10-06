      *================================================================*
      * COPYBOOK : ADLTRN                                              *
      * PURPOSE  : DEALER AUTO FINANCE DAILY TRANSACTION RECORD        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-27 TWH  ORIGINAL                                       *
      * 1995-12-19 SKN  REBATE FEE SCHEDULE UPDATE INC5881             *
      * 1995-03-28 GAF  AMOUNT-FIN ROUNDING - HALF UP CHG1890          *
      * 1996-08-13 LMB  PAYMENT EXCEPTIONS TO OPS QUEUE REQ3009        *
      * 1999-09-27 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-09-19 PQR  NEW REBATE REPORT COLUMNS INC0450              *
      * 2008-02-17 PQR  CHARGEBK NOTICE TEXT PER LEGAL CHG3720         *
      *================================================================*
       01  ADL-TRAN-REC.
           05  ADLT-KEY                    PIC X(12).
           05  ADLT-OPERATOR               PIC X(08).
           05  ADLT-REJECT-CD              PIC X(04).
           05  ADLT-MEMO                   PIC X(30).
           05  ADL-TRAN-AMOUNTS.
               10  ADLT-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  ADLT-AMOUNT-FIN-AMT     PIC S9(13)V99 COMP-3.
               10  ADLT-RESERVE-AMT        PIC S9(13)V99 COMP-3.
               10  ADLT-REBATE-AMT         PIC S9(13)V99 COMP-3.
           05  ADL-TRAN-DATES.
               10  ADLT-EFF-DATE           PIC 9(08).
               10  ADLT-POST-DATE          PIC 9(08).
           05  ADL-TRAN-CODES.
               10  ADLT-TRAN-CD            PIC X(02).
                   88  ADLT-TRAN-CD-BK             VALUE 'BK'.
                   88  ADLT-TRAN-CD-PY             VALUE 'PY'.
                   88  ADLT-TRAN-CD-CB             VALUE 'CB'.
                   88  ADLT-TRAN-CD-RB             VALUE 'RB'.
                   88  ADLT-TRAN-CD-GP             VALUE 'GP'.
               10  ADLT-CHANNEL-CD         PIC X(03).
                   88  ADLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  ADLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  ADLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  ADLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  ADLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  ADLT-REVERSAL-FLAG      PIC X(01).
                   88  ADLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  ADL-TRAN-COUNTERS.
               10  ADLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
