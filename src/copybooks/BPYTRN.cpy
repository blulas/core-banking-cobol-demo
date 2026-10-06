      *================================================================*
      * COPYBOOK : BPYTRN                                              *
      * PURPOSE  : BILL PAYMENT SERVICE DAILY TRANSACTION RECORD       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-03-10 RJT  ORIGINAL                                       *
      * 1992-03-24 TWH  FEE STATUS CODES ADDED CHG4462                 *
      * 1997-05-23 DPS  FEE FEE SCHEDULE UPDATE PRB0143                *
      * 1997-10-24 BHS  PERFORMANCE - PAYMENT READ AHEAD REQ9412       *
      * 1997-10-03 CRP  PAYMENT NOTICE TEXT PER LEGAL PRB1821          *
      * 1998-02-07 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-16 BHS  RETURNED NOTICE TEXT PER LEGAL PRB7851         *
      * 2005-06-01 DPS  FEE STATUS CODES ADDED CHG6624                 *
      * 2005-06-15 MKL  NEW REFUND REPORT COLUMNS PRB4681              *
      * 2007-11-13 GAF  PERFORMANCE - PAYMENT READ AHEAD CHG6910       *
      * 2007-05-22 CRP  RETURNED CALCULATION CORRECTED INC0162         *
      *================================================================*
       01  BPY-TRAN-REC.
           05  BPYT-KEY                    PIC X(12).
           05  BPYT-OPERATOR               PIC X(08).
           05  BPYT-REJECT-CD              PIC X(04).
           05  BPYT-MEMO                   PIC X(30).
           05  BPY-TRAN-AMOUNTS.
               10  BPYT-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  BPYT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  BPYT-REFUND-AMT         PIC S9(13)V99 COMP-3.
               10  BPYT-RETURNED-AMT       PIC S9(13)V99 COMP-3.
           05  BPY-TRAN-DATES.
               10  BPYT-EFF-DATE           PIC 9(08).
               10  BPYT-POST-DATE          PIC 9(08).
           05  BPY-TRAN-CODES.
               10  BPYT-TRAN-CD            PIC X(02).
                   88  BPYT-TRAN-CD-SC             VALUE 'SC'.
                   88  BPYT-TRAN-CD-PD             VALUE 'PD'.
                   88  BPYT-TRAN-CD-RT             VALUE 'RT'.
                   88  BPYT-TRAN-CD-CN             VALUE 'CN'.
                   88  BPYT-TRAN-CD-RF             VALUE 'RF'.
               10  BPYT-CHANNEL-CD         PIC X(03).
                   88  BPYT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  BPYT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  BPYT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  BPYT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  BPYT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  BPYT-REVERSAL-FLAG      PIC X(01).
                   88  BPYT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  BPY-TRAN-COUNTERS.
               10  BPYT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
