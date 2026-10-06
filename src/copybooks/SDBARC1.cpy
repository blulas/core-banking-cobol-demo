      *================================================================*
      * COPYBOOK : SDBARC1                                             *
      * PURPOSE  : SAFE DEPOSIT BOXES HISTORY RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-12 PQR  ORIGINAL                                       *
      * 1997-07-16 TWH  PERFORMANCE - PREPAID READ AHEAD INC4360       *
      * 1998-02-24 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-09-27 BHS  KEY-FEE NOTICE TEXT PER LEGAL INC4121          *
      * 2000-04-11 BHS  RENT EXCEPTIONS TO OPS QUEUE CHG8518           *
      * 2001-08-05 CRP  RENT LIMIT REVISED CHG9833                     *
      * 2005-04-05 CRP  KEY-FEE LIMIT REVISED REQ7375                  *
      * 2006-07-01 SKN  AUDIT FINDING - RENT CONTROL TOTALS REQ2069    *
      * 2006-03-21 SKN  PREPAID RERUN PROTECTION INC7501               *
      * 2008-01-10 EWS  NEW RENT REPORT COLUMNS PRB4291                *
      * 2009-10-21 RJT  KEY-FEE ROUNDING - HALF UP PRB9314             *
      *================================================================*
       01  SDB-H1-REC.
           05  SDBH1-KEY                   PIC X(12).
           05  SDBH1-ADDR                  PIC X(30).
           05  SDBH1-NAME                  PIC X(30).
           05  SDBH1-CUST-ID               PIC X(10).
           05  SDBH1-CONTACT               PIC X(20).
           05  SDBH1-DESC                  PIC X(30).
           05  SDBH1-REF-NO                PIC X(16).
           05  SDB-H1-AMOUNTS.
               10  SDBH1-LATE-FEE-YTD      PIC S9(13)V99 COMP-3.
               10  SDBH1-DEPOSIT-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH1-KEY-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH1-DEPOSIT-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH1-KEY-FEE-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH1-RENT-AMT          PIC S9(13)V99 COMP-3.
               10  SDBH1-LATE-FEE-AMT      PIC S9(13)V99 COMP-3.
               10  SDBH1-DRILL-AMT         PIC S9(13)V99 COMP-3.
               10  SDBH1-DRILL-YTD         PIC S9(13)V99 COMP-3.
               10  SDBH1-PREPAID-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH1-RENT-YTD          PIC S9(13)V99 COMP-3.
           05  SDB-H1-DATES.
               10  SDBH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  SDBH1-MATURITY-DATE     PIC 9(08).
               10  SDBH1-OPEN-DATE         PIC 9(08).
               10  SDBH1-LAST-ACT-DATE     PIC 9(08).
               10  SDBH1-LAST-STMT-DATE    PIC 9(08).
               10  SDBH1-NOTICE-DATE       PIC 9(08).
               10  SDBH1-LAST-MAINT-DATE   PIC 9(08).
               10  SDBH1-CLOSE-DATE        PIC 9(08).
               10  SDBH1-NEXT-DUE-DATE     PIC 9(08).
           05  SDB-H1-CODES.
               10  SDBH1-SUSPEND-FLAG      PIC X(01).
                   88  SDBH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  SDBH1-OVERRIDE-FLAG     PIC X(01).
                   88  SDBH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  SDBH1-SOURCE-CD         PIC X(02).
                   88  SDBH1-SOURCE-CD-DD          VALUE 'DD'.
                   88  SDBH1-SOURCE-CD-JZ          VALUE 'JZ'.
                   88  SDBH1-SOURCE-CD-PS          VALUE 'PS'.
                   88  SDBH1-SOURCE-CD-UM          VALUE 'UM'.
               10  SDBH1-BOX-SIZE-CD       PIC X(02).
                   88  SDBH1-BOX-SIZE-CD-CU        VALUE 'CU'.
                   88  SDBH1-BOX-SIZE-CD-MG        VALUE 'MG'.
                   88  SDBH1-BOX-SIZE-CD-VG        VALUE 'VG'.
                   88  SDBH1-BOX-SIZE-CD-VN        VALUE 'VN'.
                   88  SDBH1-BOX-SIZE-CD-XJ        VALUE 'XJ'.
               10  SDBH1-BRANCH-CD         PIC X(03).
                   88  SDBH1-BRANCH-CD-EXS         VALUE 'EXS'.
                   88  SDBH1-BRANCH-CD-FJW         VALUE 'FJW'.
                   88  SDBH1-BRANCH-CD-YFB         VALUE 'YFB'.
               10  SDBH1-CYCLE-CD          PIC X(02).
                   88  SDBH1-CYCLE-CD-AP           VALUE 'AP'.
                   88  SDBH1-CYCLE-CD-DA           VALUE 'DA'.
                   88  SDBH1-CYCLE-CD-HV           VALUE 'HV'.
                   88  SDBH1-CYCLE-CD-SE           VALUE 'SE'.
               10  SDBH1-VAULT-CD          PIC X(02).
                   88  SDBH1-VAULT-CD-BU           VALUE 'BU'.
                   88  SDBH1-VAULT-CD-ET           VALUE 'ET'.
                   88  SDBH1-VAULT-CD-FF           VALUE 'FF'.
                   88  SDBH1-VAULT-CD-NG           VALUE 'NG'.
                   88  SDBH1-VAULT-CD-NH           VALUE 'NH'.
               10  SDBH1-EXEMPT-FLAG       PIC X(01).
                   88  SDBH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  SDBH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  SDBH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  SDBH1-REVIEW-FLAG       PIC X(01).
                   88  SDBH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SDBH1-REASON-CD         PIC X(01).
               10  SDBH1-PREFERRED-FLAG    PIC X(01).
                   88  SDBH1-PREFERRED-FLAG-YES    VALUE 'Y'.
           05  SDB-H1-COUNTERS.
               10  SDBH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  SDBH1-FEE-PCT           PIC 9(03)V9(06).
               10  SDBH1-DISCOUNT          PIC 9(03)V9(06).
               10  SDBH1-PAYMENT-CNT       PIC 9(07).
               10  SDBH1-RETRY-CNT         PIC 9(07).
               10  SDBH1-ITEM-CNT          PIC 9(07).
           05  FILLER                      PIC X(20).
