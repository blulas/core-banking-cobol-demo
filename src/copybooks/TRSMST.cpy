      *================================================================*
      * COPYBOOK : TRSMST                                              *
      * PURPOSE  : PERSONAL TRUST MASTER RECORD (TRSMST KSDS)          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-09 CRP  ORIGINAL                                       *
      * 1990-07-19 MKL  INCOME RERUN PROTECTION PRB5864                *
      * 1997-02-03 SKN  NEW TRUSTEE-FEE REPORT COLUMNS PRB9736         *
      * 2001-08-19 CRP  MARKET NOTICE TEXT PER LEGAL PRB9230           *
      * 2002-01-28 BHS  TAX LIMIT REVISED PRB3804                      *
      * 2003-02-14 TWH  TRUSTEE-FEE NOTICE TEXT PER LEGAL CHG5886      *
      * 2005-10-01 LMB  PRINCIPAL STATUS CODES ADDED CHG7535           *
      * 2007-09-17 TWH  NEW DISTRIB REPORT COLUMNS PRB0862             *
      *================================================================*
       01  TRS-MASTER-REC.
           05  TRSM-KEY                    PIC X(12).
           05  TRSM-CUST-ID                PIC X(10).
           05  TRSM-NAME                   PIC X(30).
           05  TRSM-DESC                   PIC X(30).
           05  TRSM-REF-NO                 PIC X(16).
           05  TRSM-ADDR                   PIC X(30).
           05  TRSM-CONTACT                PIC X(20).
           05  TRS-MASTER-AMOUNTS.
               10  TRSM-PRINCIPAL-AMT      PIC S9(13)V99 COMP-3.
               10  TRSM-PRINCIPAL-YTD      PIC S9(13)V99 COMP-3.
               10  TRSM-INCOME-AMT         PIC S9(13)V99 COMP-3.
               10  TRSM-INCOME-YTD         PIC S9(13)V99 COMP-3.
               10  TRSM-DISTRIB-AMT        PIC S9(13)V99 COMP-3.
               10  TRSM-DISTRIB-YTD        PIC S9(13)V99 COMP-3.
               10  TRSM-TRUSTEE-FEE-AMT    PIC S9(13)V99 COMP-3.
               10  TRSM-TRUSTEE-FEE-YTD    PIC S9(13)V99 COMP-3.
               10  TRSM-MARKET-AMT         PIC S9(13)V99 COMP-3.
               10  TRSM-MARKET-YTD         PIC S9(13)V99 COMP-3.
               10  TRSM-TAX-AMT            PIC S9(13)V99 COMP-3.
               10  TRSM-TAX-YTD            PIC S9(13)V99 COMP-3.
           05  TRS-MASTER-DATES.
               10  TRSM-OPEN-DATE          PIC 9(08).
               10  TRSM-EXPIRY-DATE        PIC 9(08).
               10  TRSM-CLOSE-DATE         PIC 9(08).
               10  TRSM-LAST-PAY-DATE      PIC 9(08).
               10  TRSM-LAST-STMT-DATE     PIC 9(08).
               10  TRSM-LAST-MAINT-DATE    PIC 9(08).
               10  TRSM-MATURITY-DATE      PIC 9(08).
               10  TRSM-EFFECTIVE-DATE     PIC 9(08).
               10  TRSM-NOTICE-DATE        PIC 9(08).
           05  TRS-MASTER-CODES.
               10  TRSM-TRUST-TYPE-CD      PIC X(03).
                   88  TRSM-TRUST-TYPE-CD-FLJ      VALUE 'FLJ'.
                   88  TRSM-TRUST-TYPE-CD-MNG      VALUE 'MNG'.
                   88  TRSM-TRUST-TYPE-CD-VFN      VALUE 'VFN'.
                   88  TRSM-TRUST-TYPE-CD-XPL      VALUE 'XPL'.
               10  TRSM-DIST-FREQ-CD       PIC X(01).
                   88  TRSM-DIST-FREQ-CD-E         VALUE 'E'.
                   88  TRSM-DIST-FREQ-CD-K         VALUE 'K'.
                   88  TRSM-DIST-FREQ-CD-P         VALUE 'P'.
                   88  TRSM-DIST-FREQ-CD-W         VALUE 'W'.
                   88  TRSM-DIST-FREQ-CD-Y         VALUE 'Y'.
                   88  TRSM-DIST-FREQ-CD-Z         VALUE 'Z'.
               10  TRSM-REASON-CD          PIC X(01).
                   88  TRSM-REASON-CD-H            VALUE 'H'.
                   88  TRSM-REASON-CD-M            VALUE 'M'.
                   88  TRSM-REASON-CD-T            VALUE 'T'.
               10  TRSM-REGION-CD          PIC X(01).
                   88  TRSM-REGION-CD-A            VALUE 'A'.
                   88  TRSM-REGION-CD-D            VALUE 'D'.
                   88  TRSM-REGION-CD-N            VALUE 'N'.
                   88  TRSM-REGION-CD-T            VALUE 'T'.
               10  TRSM-STATUS-CD          PIC X(02).
                   88  TRSM-STATUS-CD-EH           VALUE 'EH'.
                   88  TRSM-STATUS-CD-GG           VALUE 'GG'.
                   88  TRSM-STATUS-CD-JG           VALUE 'JG'.
               10  TRSM-CYCLE-CD           PIC X(02).
               10  TRSM-NOTICE-SENT-FLAG   PIC X(01).
                   88  TRSM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  TRSM-SUSPEND-FLAG       PIC X(01).
                   88  TRSM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  TRSM-REVIEW-FLAG        PIC X(01).
                   88  TRSM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  TRSM-STOP-FLAG          PIC X(01).
                   88  TRSM-STOP-FLAG-YES          VALUE 'Y'.
               10  TRSM-PREFERRED-FLAG     PIC X(01).
                   88  TRSM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  TRSM-EXEMPT-FLAG        PIC X(01).
                   88  TRSM-EXEMPT-FLAG-YES        VALUE 'Y'.
           05  TRS-MASTER-COUNTERS.
               10  TRSM-CYCLE-CNT          PIC 9(07).
               10  TRSM-PAYMENT-CNT        PIC 9(07).
               10  TRSM-RETRY-CNT          PIC 9(07).
               10  TRSM-NOTICE-CNT         PIC 9(07).
               10  TRSM-DISCOUNT           PIC 9(03)V9(06).
               10  TRSM-MARGIN             PIC 9(03)V9(06).
               10  TRSM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
