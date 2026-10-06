      *================================================================*
      * COPYBOOK : TLXTRN                                              *
      * PURPOSE  : TELEX WIRE MESSAGING DAILY TRANSACTION RECORD       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-03-03 TWH  ORIGINAL                                       *
      * 1995-09-03 DPS  SETTLE CUTOFF MOVED REQ7280                    *
      * 1996-10-13 JLM  AUDIT FINDING - SETTLE CONTROL TOTALS PRB5113  *
      * 1999-03-20 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-14 PQR  CHARGE ROUNDING - HALF UP CHG4159              *
      * 2003-06-18 MKL  QUEUED FEE SCHEDULE UPDATE REQ8372             *
      * 2004-09-28 LMB  CHARGE TABLE EXPANDED CHG3770                  *
      * 2006-09-01 CRP  WIRE LIMIT REVISED CHG6567                     *
      * 2006-06-23 CRP  CHARGE RERUN PROTECTION REQ6696                *
      * 2010-05-27 GAF  CORRESP FEE SCHEDULE UPDATE PRB7616            *
      *================================================================*
       01  TLX-TRAN-REC.
           05  TLXT-KEY                    PIC X(12).
           05  TLXT-OPERATOR               PIC X(08).
           05  TLXT-REJECT-CD              PIC X(04).
           05  TLXT-MEMO                   PIC X(30).
           05  TLX-TRAN-AMOUNTS.
               10  TLXT-COVER-AMT          PIC S9(13)V99 COMP-3.
               10  TLXT-QUEUED-AMT         PIC S9(13)V99 COMP-3.
               10  TLXT-SETTLE-AMT         PIC S9(13)V99 COMP-3.
               10  TLXT-WIRE-AMT           PIC S9(13)V99 COMP-3.
           05  TLX-TRAN-DATES.
               10  TLXT-EFF-DATE           PIC 9(08).
               10  TLXT-POST-DATE          PIC 9(08).
           05  TLX-TRAN-CODES.
               10  TLXT-TRAN-CD            PIC X(02).
                   88  TLXT-TRAN-CD-IN             VALUE 'IN'.
                   88  TLXT-TRAN-CD-OT             VALUE 'OT'.
                   88  TLXT-TRAN-CD-RJ             VALUE 'RJ'.
                   88  TLXT-TRAN-CD-RP             VALUE 'RP'.
                   88  TLXT-TRAN-CD-CN             VALUE 'CN'.
               10  TLXT-CHANNEL-CD         PIC X(03).
                   88  TLXT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  TLXT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  TLXT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  TLXT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  TLXT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  TLXT-REVERSAL-FLAG      PIC X(01).
                   88  TLXT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  TLX-TRAN-COUNTERS.
               10  TLXT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
