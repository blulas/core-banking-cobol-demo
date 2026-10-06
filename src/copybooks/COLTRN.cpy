      *================================================================*
      * COPYBOOK : COLTRN                                              *
      * PURPOSE  : COLLECTIONS QUEUE DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-13 RJT  ORIGINAL                                       *
      * 1998-09-19 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-15 JLM  FEE TABLE EXPANDED INC4461                     *
      * 1999-08-02 MKL  FEE ROUNDING - HALF UP PRB2238                 *
      * 1999-02-07 SKN  PROMISED EXCEPTIONS TO OPS QUEUE PRB5106       *
      *================================================================*
       01  COL-TRAN-REC.
           05  COLT-KEY                    PIC X(12).
           05  COLT-OPERATOR               PIC X(08).
           05  COLT-REJECT-CD              PIC X(04).
           05  COLT-MEMO                   PIC X(30).
           05  COL-TRAN-AMOUNTS.
               10  COLT-COLLECTED-AMT      PIC S9(13)V99 COMP-3.
               10  COLT-WRITEOFF-AMT       PIC S9(13)V99 COMP-3.
               10  COLT-PAST-DUE-AMT       PIC S9(13)V99 COMP-3.
               10  COLT-FEE-AMT            PIC S9(13)V99 COMP-3.
           05  COL-TRAN-DATES.
               10  COLT-EFF-DATE           PIC 9(08).
               10  COLT-POST-DATE          PIC 9(08).
           05  COL-TRAN-CODES.
               10  COLT-TRAN-CD            PIC X(02).
                   88  COLT-TRAN-CD-PR             VALUE 'PR'.
                   88  COLT-TRAN-CD-CL             VALUE 'CL'.
                   88  COLT-TRAN-CD-LT             VALUE 'LT'.
                   88  COLT-TRAN-CD-WO             VALUE 'WO'.
                   88  COLT-TRAN-CD-RF             VALUE 'RF'.
               10  COLT-CHANNEL-CD         PIC X(03).
                   88  COLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  COLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  COLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  COLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  COLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  COLT-REVERSAL-FLAG      PIC X(01).
                   88  COLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  COL-TRAN-COUNTERS.
               10  COLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
