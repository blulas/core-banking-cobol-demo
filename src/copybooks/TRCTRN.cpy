      *================================================================*
      * COPYBOOK : TRCTRN                                              *
      * PURPOSE  : TRAVELERS CHEQUES DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-26 RJT  ORIGINAL                                       *
      * 1993-02-11 CRP  PERFORMANCE - CONSIGN READ AHEAD CHG8358       *
      * 1997-03-23 RJT  FACE EXCEPTIONS TO OPS QUEUE PRB2420           *
      * 1999-08-07 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-09-18 TWH  REFUND CALCULATION CORRECTED CHG9196           *
      * 2005-10-18 BHS  FACE EXCEPTIONS TO OPS QUEUE INC4931           *
      * 2006-07-17 LMB  SHORTAGE RERUN PROTECTION INC6398              *
      * 2010-04-12 LMB  SHORTAGE RERUN PROTECTION INC5237              *
      * 2012-08-09 LMB  CONSIGN STATUS CODES ADDED CHG2213             *
      * 2014-03-17 JLM  REMIT TABLE EXPANDED REQ7536                   *
      *================================================================*
       01  TRC-TRAN-REC.
           05  TRCT-KEY                    PIC X(12).
           05  TRCT-OPERATOR               PIC X(08).
           05  TRCT-REJECT-CD              PIC X(04).
           05  TRCT-MEMO                   PIC X(30).
           05  TRC-TRAN-AMOUNTS.
               10  TRCT-COMMISSION-AMT     PIC S9(13)V99 COMP-3.
               10  TRCT-CONSIGN-AMT        PIC S9(13)V99 COMP-3.
               10  TRCT-REFUND-AMT         PIC S9(13)V99 COMP-3.
               10  TRCT-REMIT-AMT          PIC S9(13)V99 COMP-3.
           05  TRC-TRAN-DATES.
               10  TRCT-EFF-DATE           PIC 9(08).
               10  TRCT-POST-DATE          PIC 9(08).
           05  TRC-TRAN-CODES.
               10  TRCT-TRAN-CD            PIC X(02).
                   88  TRCT-TRAN-CD-SL             VALUE 'SL'.
                   88  TRCT-TRAN-CD-RF             VALUE 'RF'.
                   88  TRCT-TRAN-CD-RM             VALUE 'RM'.
                   88  TRCT-TRAN-CD-CN             VALUE 'CN'.
                   88  TRCT-TRAN-CD-VD             VALUE 'VD'.
               10  TRCT-CHANNEL-CD         PIC X(03).
                   88  TRCT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  TRCT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  TRCT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  TRCT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  TRCT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  TRCT-REVERSAL-FLAG      PIC X(01).
                   88  TRCT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  TRC-TRAN-COUNTERS.
               10  TRCT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
