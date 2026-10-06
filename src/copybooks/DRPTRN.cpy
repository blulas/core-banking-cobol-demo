      *================================================================*
      * COPYBOOK : DRPTRN                                              *
      * PURPOSE  : DIVIDEND REINVESTMENT DAILY TRANSACTION RECORD      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-10-21 MKL  ORIGINAL                                       *
      * 1987-12-27 LMB  WITHHOLD STATUS CODES ADDED INC9034            *
      * 1989-05-05 TWH  AUDIT FINDING - REINVEST CONTROL TOTALS CHG6425*
      * 1991-01-28 TWH  DIVIDEND NOTICE TEXT PER LEGAL REQ6344         *
      * 1991-11-25 JLM  CASH-PAY EXCEPTIONS TO OPS QUEUE INC0384       *
      * 2006-08-19 LMB  FEE ROUNDING - HALF UP PRB1946                 *
      * 2007-09-13 LMB  FEE TABLE EXPANDED REQ0980                     *
      * 2008-03-19 EWS  DIVIDEND STATUS CODES ADDED CHG8112            *
      *================================================================*
       01  DRP-TRAN-REC.
           05  DRPT-KEY                    PIC X(12).
           05  DRPT-OPERATOR               PIC X(08).
           05  DRPT-REJECT-CD              PIC X(04).
           05  DRPT-MEMO                   PIC X(30).
           05  DRP-TRAN-AMOUNTS.
               10  DRPT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  DRPT-DIVIDEND-AMT       PIC S9(13)V99 COMP-3.
               10  DRPT-REINVEST-AMT       PIC S9(13)V99 COMP-3.
               10  DRPT-SHARES-AMT-AMT     PIC S9(13)V99 COMP-3.
           05  DRP-TRAN-DATES.
               10  DRPT-EFF-DATE           PIC 9(08).
               10  DRPT-POST-DATE          PIC 9(08).
           05  DRP-TRAN-CODES.
               10  DRPT-TRAN-CD            PIC X(02).
                   88  DRPT-TRAN-CD-DV             VALUE 'DV'.
                   88  DRPT-TRAN-CD-RI             VALUE 'RI'.
                   88  DRPT-TRAN-CD-CP             VALUE 'CP'.
                   88  DRPT-TRAN-CD-WH             VALUE 'WH'.
                   88  DRPT-TRAN-CD-SL             VALUE 'SL'.
               10  DRPT-CHANNEL-CD         PIC X(03).
                   88  DRPT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  DRPT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  DRPT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  DRPT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  DRPT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  DRPT-REVERSAL-FLAG      PIC X(01).
                   88  DRPT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  DRP-TRAN-COUNTERS.
               10  DRPT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
