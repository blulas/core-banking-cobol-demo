      *================================================================*
      * COPYBOOK : LBXTRN                                              *
      * PURPOSE  : LOCKBOX REMITTANCE DAILY TRANSACTION RECORD         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-02 JLM  ORIGINAL                                       *
      * 1996-06-18 RJT  REMIT EXCEPTIONS TO OPS QUEUE PRB5229          *
      * 1997-06-09 LMB  DEPOSIT CALCULATION CORRECTED PRB3623          *
      * 1998-10-03 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2012-09-14 LMB  FEE EXCEPTIONS TO OPS QUEUE REQ3040            *
      *================================================================*
       01  LBX-TRAN-REC.
           05  LBXT-KEY                    PIC X(12).
           05  LBXT-OPERATOR               PIC X(08).
           05  LBXT-REJECT-CD              PIC X(04).
           05  LBXT-MEMO                   PIC X(30).
           05  LBX-TRAN-AMOUNTS.
               10  LBXT-FLOAT-AMT          PIC S9(13)V99 COMP-3.
               10  LBXT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  LBXT-CASH-ON-HAND-AMT   PIC S9(13)V99 COMP-3.
               10  LBXT-CHECK-AMT          PIC S9(13)V99 COMP-3.
           05  LBX-TRAN-DATES.
               10  LBXT-EFF-DATE           PIC 9(08).
               10  LBXT-POST-DATE          PIC 9(08).
           05  LBX-TRAN-CODES.
               10  LBXT-TRAN-CD            PIC X(02).
                   88  LBXT-TRAN-CD-RC             VALUE 'RC'.
                   88  LBXT-TRAN-CD-DP             VALUE 'DP'.
                   88  LBXT-TRAN-CD-EX             VALUE 'EX'.
                   88  LBXT-TRAN-CD-RJ             VALUE 'RJ'.
                   88  LBXT-TRAN-CD-AD             VALUE 'AD'.
               10  LBXT-CHANNEL-CD         PIC X(03).
                   88  LBXT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  LBXT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  LBXT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  LBXT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  LBXT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  LBXT-REVERSAL-FLAG      PIC X(01).
                   88  LBXT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  LBX-TRAN-COUNTERS.
               10  LBXT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
