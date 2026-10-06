      *================================================================*
      * COPYBOOK : SDBTRN                                              *
      * PURPOSE  : SAFE DEPOSIT BOXES DAILY TRANSACTION RECORD         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-27 CRP  ORIGINAL                                       *
      * 1996-12-06 CRP  DRILL FEE SCHEDULE UPDATE INC5942              *
      * 1998-02-13 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-26 MKL  DEPOSIT NOTICE TEXT PER LEGAL CHG3580          *
      * 2001-02-25 JLM  DRILL NOTICE TEXT PER LEGAL INC9584            *
      * 2002-03-28 PQR  DRILL FEE SCHEDULE UPDATE PRB5271              *
      * 2003-02-12 BHS  LATE-FEE STATUS CODES ADDED INC6460            *
      * 2004-11-16 GAF  KEY-FEE CUTOFF MOVED INC0515                   *
      * 2005-04-14 JLM  DRILL FEE SCHEDULE UPDATE PRB3204              *
      * 2008-12-15 TWH  DEPOSIT STATUS CODES ADDED INC9400             *
      * 2008-07-16 SKN  LATE-FEE NOTICE TEXT PER LEGAL PRB8020         *
      * 2008-05-11 BHS  PREPAID RERUN PROTECTION PRB9419               *
      * 2009-05-17 JLM  AUDIT FINDING - LATE-FEE CONTROL TOTALS REQ4224*
      *================================================================*
       01  SDB-TRAN-REC.
           05  SDBT-KEY                    PIC X(12).
           05  SDBT-OPERATOR               PIC X(08).
           05  SDBT-REJECT-CD              PIC X(04).
           05  SDBT-MEMO                   PIC X(30).
           05  SDB-TRAN-AMOUNTS.
               10  SDBT-KEY-FEE-AMT        PIC S9(13)V99 COMP-3.
               10  SDBT-DRILL-AMT          PIC S9(13)V99 COMP-3.
               10  SDBT-DEPOSIT-AMT        PIC S9(13)V99 COMP-3.
               10  SDBT-RENT-AMT           PIC S9(13)V99 COMP-3.
           05  SDB-TRAN-DATES.
               10  SDBT-EFF-DATE           PIC 9(08).
               10  SDBT-POST-DATE          PIC 9(08).
           05  SDB-TRAN-CODES.
               10  SDBT-TRAN-CD            PIC X(02).
                   88  SDBT-TRAN-CD-RN             VALUE 'RN'.
                   88  SDBT-TRAN-CD-AC             VALUE 'AC'.
                   88  SDBT-TRAN-CD-DR             VALUE 'DR'.
                   88  SDBT-TRAN-CD-KY             VALUE 'KY'.
                   88  SDBT-TRAN-CD-CL             VALUE 'CL'.
               10  SDBT-CHANNEL-CD         PIC X(03).
                   88  SDBT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  SDBT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  SDBT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  SDBT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  SDBT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  SDBT-REVERSAL-FLAG      PIC X(01).
                   88  SDBT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  SDB-TRAN-COUNTERS.
               10  SDBT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
