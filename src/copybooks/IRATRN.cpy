      *================================================================*
      * COPYBOOK : IRATRN                                              *
      * PURPOSE  : IRA CUSTODY DAILY TRANSACTION RECORD                *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-24 SKN  ORIGINAL                                       *
      * 2007-08-13 SKN  CONTRIB EXCEPTIONS TO OPS QUEUE PRB3471        *
      * 2008-04-03 SKN  FMV RERUN PROTECTION REQ7798                   *
      * 2009-10-03 BHS  ROLLOVER EXCEPTIONS TO OPS QUEUE CHG2455       *
      * 2011-12-15 CRP  CONTRIB CUTOFF MOVED PRB3335                   *
      *================================================================*
       01  IRA-TRAN-REC.
           05  IRAT-KEY                    PIC X(12).
           05  IRAT-OPERATOR               PIC X(08).
           05  IRAT-REJECT-CD              PIC X(04).
           05  IRAT-MEMO                   PIC X(30).
           05  IRA-TRAN-AMOUNTS.
               10  IRAT-DISTRIB-AMT        PIC S9(13)V99 COMP-3.
               10  IRAT-ROLLOVER-AMT       PIC S9(13)V99 COMP-3.
               10  IRAT-RMD-AMT            PIC S9(13)V99 COMP-3.
               10  IRAT-EXCESS-AMT         PIC S9(13)V99 COMP-3.
           05  IRA-TRAN-DATES.
               10  IRAT-EFF-DATE           PIC 9(08).
               10  IRAT-POST-DATE          PIC 9(08).
           05  IRA-TRAN-CODES.
               10  IRAT-TRAN-CD            PIC X(02).
                   88  IRAT-TRAN-CD-CN             VALUE 'CN'.
                   88  IRAT-TRAN-CD-DS             VALUE 'DS'.
                   88  IRAT-TRAN-CD-RO             VALUE 'RO'.
                   88  IRAT-TRAN-CD-RC             VALUE 'RC'.
                   88  IRAT-TRAN-CD-WH             VALUE 'WH'.
               10  IRAT-CHANNEL-CD         PIC X(03).
                   88  IRAT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  IRAT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  IRAT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  IRAT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  IRAT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  IRAT-REVERSAL-FLAG      PIC X(01).
                   88  IRAT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  IRA-TRAN-COUNTERS.
               10  IRAT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
