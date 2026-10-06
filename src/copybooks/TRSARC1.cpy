      *================================================================*
      * COPYBOOK : TRSARC1                                             *
      * PURPOSE  : PERSONAL TRUST ARCHIVE RECORD LAYOUT                *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-05-17 LMB  ORIGINAL                                       *
      * 1988-05-04 LMB  PRINCIPAL STATUS CODES ADDED INC2874           *
      * 1996-04-19 EWS  AUDIT FINDING - INCOME CONTROL TOTALS REQ8180  *
      * 2002-11-08 CRP  NEW TAX REPORT COLUMNS INC7061                 *
      * 2003-10-04 TWH  INCOME NOTICE TEXT PER LEGAL PRB8310           *
      * 2007-08-09 MKL  TRUSTEE-FEE NOTICE TEXT PER LEGAL REQ2147      *
      *================================================================*
       01  TRS-H1-REC.
           05  TRSH1-CUST-ID               PIC X(10).
           05  TRSH1-KEY                   PIC X(12).
           05  TRSH1-CONTACT               PIC X(20).
           05  TRSH1-NAME                  PIC X(30).
           05  TRSH1-DESC                  PIC X(30).
           05  TRS-H1-AMOUNTS.
               10  TRSH1-PRINCIPAL-AMT     PIC S9(13)V99 COMP-3.
               10  TRSH1-DISTRIB-AMT       PIC S9(13)V99 COMP-3.
               10  TRSH1-INCOME-AMT        PIC S9(13)V99 COMP-3.
               10  TRSH1-TRUSTEE-FEE-AMT   PIC S9(13)V99 COMP-3.
               10  TRSH1-TAX-YTD           PIC S9(13)V99 COMP-3.
               10  TRSH1-INCOME-YTD        PIC S9(13)V99 COMP-3.
               10  TRSH1-DISTRIB-YTD       PIC S9(13)V99 COMP-3.
               10  TRSH1-PRINCIPAL-YTD     PIC S9(13)V99 COMP-3.
               10  TRSH1-TAX-AMT           PIC S9(13)V99 COMP-3.
               10  TRSH1-MARKET-AMT        PIC S9(13)V99 COMP-3.
           05  TRS-H1-DATES.
               10  TRSH1-CLOSE-DATE        PIC 9(08).
               10  TRSH1-LAST-PAY-DATE     PIC 9(08).
               10  TRSH1-EFFECTIVE-DATE    PIC 9(08).
               10  TRSH1-EXPIRY-DATE       PIC 9(08).
               10  TRSH1-LAST-STMT-DATE    PIC 9(08).
               10  TRSH1-MATURITY-DATE     PIC 9(08).
               10  TRSH1-NOTICE-DATE       PIC 9(08).
           05  TRS-H1-CODES.
               10  TRSH1-PREFERRED-FLAG    PIC X(01).
                   88  TRSH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  TRSH1-TRUST-TYPE-CD     PIC X(03).
                   88  TRSH1-TRUST-TYPE-CD-FLJ     VALUE 'FLJ'.
                   88  TRSH1-TRUST-TYPE-CD-MNG     VALUE 'MNG'.
                   88  TRSH1-TRUST-TYPE-CD-VFN     VALUE 'VFN'.
                   88  TRSH1-TRUST-TYPE-CD-XPL     VALUE 'XPL'.
               10  TRSH1-EXEMPT-FLAG       PIC X(01).
                   88  TRSH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TRSH1-SUSPEND-FLAG      PIC X(01).
                   88  TRSH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TRSH1-STOP-FLAG         PIC X(01).
                   88  TRSH1-STOP-FLAG-YES         VALUE 'Y'.
               10  TRSH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  TRSH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TRSH1-REVIEW-FLAG       PIC X(01).
                   88  TRSH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TRSH1-REASON-CD         PIC X(01).
                   88  TRSH1-REASON-CD-H           VALUE 'H'.
                   88  TRSH1-REASON-CD-M           VALUE 'M'.
                   88  TRSH1-REASON-CD-T           VALUE 'T'.
               10  TRSH1-CYCLE-CD          PIC X(02).
               10  TRSH1-STATUS-CD         PIC X(02).
                   88  TRSH1-STATUS-CD-EH          VALUE 'EH'.
                   88  TRSH1-STATUS-CD-GG          VALUE 'GG'.
                   88  TRSH1-STATUS-CD-JG          VALUE 'JG'.
               10  TRSH1-REGION-CD         PIC X(01).
                   88  TRSH1-REGION-CD-A           VALUE 'A'.
                   88  TRSH1-REGION-CD-D           VALUE 'D'.
                   88  TRSH1-REGION-CD-N           VALUE 'N'.
                   88  TRSH1-REGION-CD-T           VALUE 'T'.
           05  TRS-H1-COUNTERS.
               10  TRSH1-MARGIN            PIC 9(03)V9(06).
               10  TRSH1-DISCOUNT          PIC 9(03)V9(06).
               10  TRSH1-RETRY-CNT         PIC 9(07).
               10  TRSH1-PAYMENT-CNT       PIC 9(07).
               10  TRSH1-NOTICE-CNT        PIC 9(07).
               10  TRSH1-CYCLE-CNT         PIC 9(07).
               10  TRSH1-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
