      *================================================================*
      * COPYBOOK : FXNTRN                                              *
      * PURPOSE  : FOREIGN BANKNOTES DAILY TRANSACTION RECORD          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-28 BHS  ORIGINAL                                       *
      * 1995-10-12 TWH  SHIPPING FEE SCHEDULE UPDATE REQ2768           *
      * 1997-02-15 RJT  BUYBACK RERUN PROTECTION REQ7828               *
      * 1998-06-08 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-24 LMB  AUDIT FINDING - USD CONTROL TOTALS REQ1203     *
      * 2000-10-15 CRP  NEW FOREIGN REPORT COLUMNS PRB0980             *
      * 2001-08-06 GAF  BUYBACK NOTICE TEXT PER LEGAL CHG1363          *
      * 2002-02-14 EWS  FOREIGN ROUNDING - HALF UP CHG7523             *
      * 2004-11-27 EWS  PERFORMANCE - USD READ AHEAD PRB7582           *
      * 2005-12-06 PQR  FOREIGN CALCULATION CORRECTED REQ5067          *
      * 2005-09-15 DPS  BUYBACK FEE SCHEDULE UPDATE REQ9690            *
      *================================================================*
       01  FXN-TRAN-REC.
           05  FXNT-KEY                    PIC X(12).
           05  FXNT-OPERATOR               PIC X(08).
           05  FXNT-REJECT-CD              PIC X(04).
           05  FXNT-MEMO                   PIC X(30).
           05  FXN-TRAN-AMOUNTS.
               10  FXNT-MARGIN-AMT         PIC S9(13)V99 COMP-3.
               10  FXNT-BUYBACK-AMT        PIC S9(13)V99 COMP-3.
               10  FXNT-FOREIGN-AMT        PIC S9(13)V99 COMP-3.
               10  FXNT-USD-AMT            PIC S9(13)V99 COMP-3.
           05  FXN-TRAN-DATES.
               10  FXNT-EFF-DATE           PIC 9(08).
               10  FXNT-POST-DATE          PIC 9(08).
           05  FXN-TRAN-CODES.
               10  FXNT-TRAN-CD            PIC X(02).
                   88  FXNT-TRAN-CD-OR             VALUE 'OR'.
                   88  FXNT-TRAN-CD-BB             VALUE 'BB'.
                   88  FXNT-TRAN-CD-SH             VALUE 'SH'.
                   88  FXNT-TRAN-CD-CN             VALUE 'CN'.
               10  FXNT-CHANNEL-CD         PIC X(03).
                   88  FXNT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  FXNT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  FXNT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  FXNT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  FXNT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  FXNT-REVERSAL-FLAG      PIC X(01).
                   88  FXNT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  FXN-TRAN-COUNTERS.
               10  FXNT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
