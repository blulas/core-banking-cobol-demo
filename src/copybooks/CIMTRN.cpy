      *================================================================*
      * COPYBOOK : CIMTRN                                              *
      * PURPOSE  : CHECK IMAGE ARCHIVE DAILY TRANSACTION RECORD        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-10-28 MKL  ORIGINAL                                       *
      * 1996-11-21 JLM  ITEM CUTOFF MOVED INC7502                      *
      * 1998-09-07 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-07 JLM  RETRIEVE-FEE ROUNDING - HALF UP CHG7500        *
      * 2005-04-08 DPS  STORAGE EXCEPTIONS TO OPS QUEUE PRB7300        *
      *================================================================*
       01  CIM-TRAN-REC.
           05  CIMT-KEY                    PIC X(12).
           05  CIMT-OPERATOR               PIC X(08).
           05  CIMT-REJECT-CD              PIC X(04).
           05  CIMT-MEMO                   PIC X(30).
           05  CIM-TRAN-AMOUNTS.
               10  CIMT-STORAGE-AMT        PIC S9(13)V99 COMP-3.
               10  CIMT-IMAGE-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  CIMT-ITEM-AMT           PIC S9(13)V99 COMP-3.
               10  CIMT-RETRIEVE-FEE-AMT   PIC S9(13)V99 COMP-3.
           05  CIM-TRAN-DATES.
               10  CIMT-EFF-DATE           PIC 9(08).
               10  CIMT-POST-DATE          PIC 9(08).
           05  CIM-TRAN-CODES.
               10  CIMT-TRAN-CD            PIC X(02).
                   88  CIMT-TRAN-CD-CP             VALUE 'CP'.
                   88  CIMT-TRAN-CD-RT             VALUE 'RT'.
                   88  CIMT-TRAN-CD-ST             VALUE 'ST'.
                   88  CIMT-TRAN-CD-PG             VALUE 'PG'.
               10  CIMT-CHANNEL-CD         PIC X(03).
                   88  CIMT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  CIMT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  CIMT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  CIMT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  CIMT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  CIMT-REVERSAL-FLAG      PIC X(01).
                   88  CIMT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  CIM-TRAN-COUNTERS.
               10  CIMT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
