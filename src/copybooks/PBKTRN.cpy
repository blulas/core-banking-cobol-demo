      *================================================================*
      * COPYBOOK : PBKTRN                                              *
      * PURPOSE  : PASSBOOK PRINTING DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-02 CRP  ORIGINAL                                       *
      * 1998-03-20 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-04 TWH  PERFORMANCE - LINE READ AHEAD REQ3296          *
      * 2003-04-20 PQR  CARRYFWD LIMIT REVISED PRB8242                 *
      * 2003-10-28 RJT  BALANCE NOTICE TEXT PER LEGAL REQ0841          *
      * 2003-09-20 TWH  PERFORMANCE - UNPOSTED READ AHEAD INC7741      *
      * 2005-02-13 MKL  CARRYFWD STATUS CODES ADDED INC3148            *
      * 2005-06-16 EWS  BALANCE STATUS CODES ADDED CHG9919             *
      * 2008-01-19 LMB  AUDIT FINDING - BALANCE CONTROL TOTALS CHG4227 *
      * 2009-09-13 GAF  BALANCE EXCEPTIONS TO OPS QUEUE REQ8572        *
      * 2012-03-25 JLM  UNPOSTED CUTOFF MOVED REQ2167                  *
      * 2013-06-02 DPS  PRINTED NOTICE TEXT PER LEGAL INC1596          *
      *================================================================*
       01  PBK-TRAN-REC.
           05  PBKT-KEY                    PIC X(12).
           05  PBKT-OPERATOR               PIC X(08).
           05  PBKT-REJECT-CD              PIC X(04).
           05  PBKT-MEMO                   PIC X(30).
           05  PBK-TRAN-AMOUNTS.
               10  PBKT-PRINTED-AMT        PIC S9(13)V99 COMP-3.
               10  PBKT-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  PBKT-LINE-AMT           PIC S9(13)V99 COMP-3.
               10  PBKT-CARRYFWD-AMT       PIC S9(13)V99 COMP-3.
           05  PBK-TRAN-DATES.
               10  PBKT-EFF-DATE           PIC 9(08).
               10  PBKT-POST-DATE          PIC 9(08).
           05  PBK-TRAN-CODES.
               10  PBKT-TRAN-CD            PIC X(02).
                   88  PBKT-TRAN-CD-UP             VALUE 'UP'.
                   88  PBKT-TRAN-CD-PR             VALUE 'PR'.
                   88  PBKT-TRAN-CD-RB             VALUE 'RB'.
                   88  PBKT-TRAN-CD-LS             VALUE 'LS'.
                   88  PBKT-TRAN-CD-CF             VALUE 'CF'.
               10  PBKT-CHANNEL-CD         PIC X(03).
                   88  PBKT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  PBKT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  PBKT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  PBKT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  PBKT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  PBKT-REVERSAL-FLAG      PIC X(01).
                   88  PBKT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  PBK-TRAN-COUNTERS.
               10  PBKT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
