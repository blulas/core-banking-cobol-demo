      *================================================================*
      * COPYBOOK : MORTRN                                              *
      * PURPOSE  : MONEY ORDERS DAILY TRANSACTION RECORD               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-16 SKN  ORIGINAL                                       *
      * 1994-01-16 CRP  PAID ROUNDING - HALF UP INC5727                *
      * 1998-07-19 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-28 TWH  NEW OUTSTAND REPORT COLUMNS PRB0243            *
      * 2002-04-27 BHS  OUTSTAND LIMIT REVISED INC0769                 *
      * 2003-10-28 EWS  OUTSTAND LIMIT REVISED CHG8938                 *
      * 2004-06-05 BHS  FACE RERUN PROTECTION CHG1554                  *
      * 2006-01-25 TWH  FEE ROUNDING - HALF UP PRB4231                 *
      * 2006-04-27 TWH  ESCHEAT STATUS CODES ADDED CHG5356             *
      *================================================================*
       01  MOR-TRAN-REC.
           05  MORT-KEY                    PIC X(12).
           05  MORT-OPERATOR               PIC X(08).
           05  MORT-REJECT-CD              PIC X(04).
           05  MORT-MEMO                   PIC X(30).
           05  MOR-TRAN-AMOUNTS.
               10  MORT-FACE-AMT           PIC S9(13)V99 COMP-3.
               10  MORT-OUTSTAND-AMT       PIC S9(13)V99 COMP-3.
               10  MORT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  MORT-ESCHEAT-AMT        PIC S9(13)V99 COMP-3.
           05  MOR-TRAN-DATES.
               10  MORT-EFF-DATE           PIC 9(08).
               10  MORT-POST-DATE          PIC 9(08).
           05  MOR-TRAN-CODES.
               10  MORT-TRAN-CD            PIC X(02).
                   88  MORT-TRAN-CD-IS             VALUE 'IS'.
                   88  MORT-TRAN-CD-PD             VALUE 'PD'.
                   88  MORT-TRAN-CD-VD             VALUE 'VD'.
                   88  MORT-TRAN-CD-SP             VALUE 'SP'.
                   88  MORT-TRAN-CD-ES             VALUE 'ES'.
               10  MORT-CHANNEL-CD         PIC X(03).
                   88  MORT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  MORT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  MORT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  MORT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  MORT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  MORT-REVERSAL-FLAG      PIC X(01).
                   88  MORT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  MOR-TRAN-COUNTERS.
               10  MORT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
