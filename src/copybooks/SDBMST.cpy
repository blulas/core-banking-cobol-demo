      *================================================================*
      * COPYBOOK : SDBMST                                              *
      * PURPOSE  : SAFE DEPOSIT BOXES MASTER RECORD (SDBMST KSDS)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-09-11 LMB  ORIGINAL                                       *
      * 1997-05-03 MKL  DRILL RERUN PROTECTION REQ7578                 *
      * 1999-01-27 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-03 CRP  AUDIT FINDING - RENT CONTROL TOTALS INC1380    *
      * 2004-06-02 EWS  NEW PREPAID REPORT COLUMNS PRB9595             *
      *================================================================*
       01  SDB-MASTER-REC.
           05  SDBM-KEY                    PIC X(12).
           05  SDBM-CUST-ID                PIC X(10).
           05  SDBM-NAME                   PIC X(30).
           05  SDBM-DESC                   PIC X(30).
           05  SDBM-REF-NO                 PIC X(16).
           05  SDBM-ADDR                   PIC X(30).
           05  SDBM-CONTACT                PIC X(20).
           05  SDB-MASTER-AMOUNTS.
               10  SDBM-RENT-AMT           PIC S9(13)V99 COMP-3.
               10  SDBM-RENT-YTD           PIC S9(13)V99 COMP-3.
               10  SDBM-DEPOSIT-AMT        PIC S9(13)V99 COMP-3.
               10  SDBM-DEPOSIT-YTD        PIC S9(13)V99 COMP-3.
               10  SDBM-DRILL-AMT          PIC S9(13)V99 COMP-3.
               10  SDBM-DRILL-YTD          PIC S9(13)V99 COMP-3.
               10  SDBM-LATE-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  SDBM-LATE-FEE-YTD       PIC S9(13)V99 COMP-3.
               10  SDBM-KEY-FEE-AMT        PIC S9(13)V99 COMP-3.
               10  SDBM-KEY-FEE-YTD        PIC S9(13)V99 COMP-3.
               10  SDBM-PREPAID-AMT        PIC S9(13)V99 COMP-3.
               10  SDBM-PREPAID-YTD        PIC S9(13)V99 COMP-3.
           05  SDB-MASTER-DATES.
               10  SDBM-NOTICE-DATE        PIC 9(08).
               10  SDBM-LAST-MAINT-DATE    PIC 9(08).
               10  SDBM-LAST-ACT-DATE      PIC 9(08).
               10  SDBM-NEXT-REVIEW-DATE   PIC 9(08).
               10  SDBM-NEXT-DUE-DATE      PIC 9(08).
               10  SDBM-MATURITY-DATE      PIC 9(08).
               10  SDBM-CLOSE-DATE         PIC 9(08).
               10  SDBM-LAST-STMT-DATE     PIC 9(08).
               10  SDBM-OPEN-DATE          PIC 9(08).
           05  SDB-MASTER-CODES.
               10  SDBM-BOX-SIZE-CD        PIC X(02).
                   88  SDBM-BOX-SIZE-CD-CU         VALUE 'CU'.
                   88  SDBM-BOX-SIZE-CD-MG         VALUE 'MG'.
                   88  SDBM-BOX-SIZE-CD-VG         VALUE 'VG'.
                   88  SDBM-BOX-SIZE-CD-VN         VALUE 'VN'.
                   88  SDBM-BOX-SIZE-CD-XJ         VALUE 'XJ'.
               10  SDBM-VAULT-CD           PIC X(02).
                   88  SDBM-VAULT-CD-BU            VALUE 'BU'.
                   88  SDBM-VAULT-CD-ET            VALUE 'ET'.
                   88  SDBM-VAULT-CD-FF            VALUE 'FF'.
                   88  SDBM-VAULT-CD-NG            VALUE 'NG'.
                   88  SDBM-VAULT-CD-NH            VALUE 'NH'.
               10  SDBM-BRANCH-CD          PIC X(03).
                   88  SDBM-BRANCH-CD-EXS          VALUE 'EXS'.
                   88  SDBM-BRANCH-CD-FJW          VALUE 'FJW'.
                   88  SDBM-BRANCH-CD-YFB          VALUE 'YFB'.
               10  SDBM-REASON-CD          PIC X(01).
               10  SDBM-CYCLE-CD           PIC X(02).
                   88  SDBM-CYCLE-CD-AP            VALUE 'AP'.
                   88  SDBM-CYCLE-CD-DA            VALUE 'DA'.
                   88  SDBM-CYCLE-CD-HV            VALUE 'HV'.
                   88  SDBM-CYCLE-CD-SE            VALUE 'SE'.
               10  SDBM-SOURCE-CD          PIC X(02).
                   88  SDBM-SOURCE-CD-DD           VALUE 'DD'.
                   88  SDBM-SOURCE-CD-JZ           VALUE 'JZ'.
                   88  SDBM-SOURCE-CD-PS           VALUE 'PS'.
                   88  SDBM-SOURCE-CD-UM           VALUE 'UM'.
               10  SDBM-OVERRIDE-FLAG      PIC X(01).
                   88  SDBM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  SDBM-SUSPEND-FLAG       PIC X(01).
                   88  SDBM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  SDBM-PREFERRED-FLAG     PIC X(01).
                   88  SDBM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  SDBM-AUTO-RENEW-FLAG    PIC X(01).
                   88  SDBM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  SDBM-EXEMPT-FLAG        PIC X(01).
                   88  SDBM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  SDBM-REVIEW-FLAG        PIC X(01).
                   88  SDBM-REVIEW-FLAG-YES        VALUE 'Y'.
           05  SDB-MASTER-COUNTERS.
               10  SDBM-RETRY-CNT          PIC 9(07).
               10  SDBM-NOTICE-CNT         PIC 9(07).
               10  SDBM-PAYMENT-CNT        PIC 9(07).
               10  SDBM-ITEM-CNT           PIC 9(07).
               10  SDBM-DISCOUNT           PIC 9(03)V9(06).
               10  SDBM-FEE-PCT            PIC 9(03)V9(06).
               10  SDBM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
