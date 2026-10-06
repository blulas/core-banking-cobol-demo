      *================================================================*
      * COPYBOOK : PRLTRN                                              *
      * PURPOSE  : PAYROLL SERVICES DAILY TRANSACTION RECORD           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-24 TWH  ORIGINAL                                       *
      * 1992-06-23 EWS  STATE-TAX EXCEPTIONS TO OPS QUEUE CHG7948      *
      * 1993-08-14 PQR  FICA ROUNDING - HALF UP REQ3759                *
      * 2008-01-13 EWS  STATE-TAX CUTOFF MOVED CHG3420                 *
      * 2008-12-21 MKL  GROSS FEE SCHEDULE UPDATE INC9686              *
      *================================================================*
       01  PRL-TRAN-REC.
           05  PRLT-KEY                    PIC X(12).
           05  PRLT-OPERATOR               PIC X(08).
           05  PRLT-REJECT-CD              PIC X(04).
           05  PRLT-MEMO                   PIC X(30).
           05  PRL-TRAN-AMOUNTS.
               10  PRLT-DEDUCT-AMT         PIC S9(13)V99 COMP-3.
               10  PRLT-STATE-TAX-AMT      PIC S9(13)V99 COMP-3.
               10  PRLT-FICA-AMT           PIC S9(13)V99 COMP-3.
               10  PRLT-NET-AMT            PIC S9(13)V99 COMP-3.
           05  PRL-TRAN-DATES.
               10  PRLT-EFF-DATE           PIC 9(08).
               10  PRLT-POST-DATE          PIC 9(08).
           05  PRL-TRAN-CODES.
               10  PRLT-TRAN-CD            PIC X(02).
                   88  PRLT-TRAN-CD-PR             VALUE 'PR'.
                   88  PRLT-TRAN-CD-AD             VALUE 'AD'.
                   88  PRLT-TRAN-CD-VD             VALUE 'VD'.
                   88  PRLT-TRAN-CD-TX             VALUE 'TX'.
                   88  PRLT-TRAN-CD-DP             VALUE 'DP'.
               10  PRLT-CHANNEL-CD         PIC X(03).
                   88  PRLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  PRLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  PRLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  PRLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  PRLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  PRLT-REVERSAL-FLAG      PIC X(01).
                   88  PRLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  PRL-TRAN-COUNTERS.
               10  PRLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
