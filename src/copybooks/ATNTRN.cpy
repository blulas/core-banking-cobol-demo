      *================================================================*
      * COPYBOOK : ATNTRN                                              *
      * PURPOSE  : ATM NETWORK SETTLEMENT DAILY TRANSACTION RECORD     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-22 JLM  ORIGINAL                                       *
      * 1998-08-03 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-21 TWH  PERFORMANCE - DISPENSED READ AHEAD PRB6859     *
      * 2002-08-01 TWH  AUDIT FINDING - SETTLE CONTROL TOTALS REQ6923  *
      * 2011-04-08 DPS  SETTLE STATUS CODES ADDED PRB1292              *
      * 2011-03-26 MKL  AUDIT FINDING - INTERCHG CONTROL TOTALS PRB5799*
      * 2013-05-20 GAF  INTERCHG RERUN PROTECTION REQ5211              *
      *================================================================*
       01  ATN-TRAN-REC.
           05  ATNT-KEY                    PIC X(12).
           05  ATNT-OPERATOR               PIC X(08).
           05  ATNT-REJECT-CD              PIC X(04).
           05  ATNT-MEMO                   PIC X(30).
           05  ATN-TRAN-AMOUNTS.
               10  ATNT-SURCHG-AMT         PIC S9(13)V99 COMP-3.
               10  ATNT-CASSETTE-AMT       PIC S9(13)V99 COMP-3.
               10  ATNT-DISPENSED-AMT      PIC S9(13)V99 COMP-3.
               10  ATNT-SETTLE-AMT         PIC S9(13)V99 COMP-3.
           05  ATN-TRAN-DATES.
               10  ATNT-EFF-DATE           PIC 9(08).
               10  ATNT-POST-DATE          PIC 9(08).
           05  ATN-TRAN-CODES.
               10  ATNT-TRAN-CD            PIC X(02).
                   88  ATNT-TRAN-CD-WD             VALUE 'WD'.
                   88  ATNT-TRAN-CD-BI             VALUE 'BI'.
                   88  ATNT-TRAN-CD-TR             VALUE 'TR'.
                   88  ATNT-TRAN-CD-AD             VALUE 'AD'.
                   88  ATNT-TRAN-CD-RV             VALUE 'RV'.
               10  ATNT-CHANNEL-CD         PIC X(03).
                   88  ATNT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  ATNT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  ATNT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  ATNT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  ATNT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  ATNT-REVERSAL-FLAG      PIC X(01).
                   88  ATNT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  ATN-TRAN-COUNTERS.
               10  ATNT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
